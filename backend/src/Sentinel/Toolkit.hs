-- | Toolkit: A collection of tools with shared configuration.
--
-- A Toolkit bundles together:
-- - A list of tools (with guards and execution logic)
-- - A system prompt for the LLM
-- - Helper functions for tool lookup and execution
module Sentinel.Toolkit
  ( -- * Toolkit Type
    Toolkit (..),

    -- * Toolkit Operations
    lookupTool,
    dataTools,
    actionTools,
    toLLMTools,

    -- * Building a Sentinel from a Toolkit
    toolkitSentinel,

    -- * Helper for tool execution callback
    makeRunDataTool,

    -- * Verification Support
    withVerification,
    extractVerifiableClaims,
  )
where

import Data.Aeson (Value)
import Data.Aeson.Text qualified as Aeson.Text
import Data.IORef (readIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Pre
import Sentinel.Context (ContextDecls)
import Sentinel.Facts qualified as Facts
import Sentinel.Output qualified as Output
import Sentinel.Sentinel (Sentinel (..), SentinelEnv (..), SentinelM, SentinelResult (..), UserQuestion (..), addFacts, getAskableStore, getContextStore)
import Sentinel.Solver (runSolver)
import Sentinel.Solver.Askable (AskableRegistry)
import Sentinel.Solver.Combinators (SolverEnv (..), emptySolverState, withRule)
import Sentinel.Solver.ToolBindings (ToolBindingRegistry)
import Sentinel.Solver.Types
  ( AskableBlock (..),
    BaseFact,
    ContextBlock (..),
    FailurePath (..),
    Proof (..),
    SolverResult (..),
    SolverSuccess (..),
  )
import Sentinel.Tool (LLMTool, Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..), toLLMTool)

--------------------------------------------------------------------------------
-- Toolkit Type
--------------------------------------------------------------------------------

-- | A toolkit bundles tools with a system prompt.
--
-- Type parameter:
-- - @db@: The database type (e.g., AirlineDB)
data Toolkit db = Toolkit
  { -- | All tools in this toolkit
    tools :: [Tool db],
    -- | System prompt for the LLM
    systemPrompt :: Text,
    -- | Tool bindings for the solver (predicate -> tool mappings)
    toolBindings :: ToolBindingRegistry,
    -- | Askable predicate declarations (user-confirmable facts)
    askables :: AskableRegistry,
    -- | Context variable declarations (conversational focus)
    contextDecls :: ContextDecls
  }

--------------------------------------------------------------------------------
-- Toolkit Operations
--------------------------------------------------------------------------------

-- | Look up a tool by name.
lookupTool :: Text -> Toolkit db -> Maybe (Tool db)
lookupTool toolName toolkit =
  find (\t -> t.name == toolName) toolkit.tools

-- | Get all data tools (can be auto-invoked by guards).
dataTools :: Toolkit db -> [Tool db]
dataTools toolkit =
  filter (\t -> t.category == DataTool) toolkit.tools

-- | Get all action tools (require explicit LLM invocation).
actionTools :: Toolkit db -> [Tool db]
actionTools toolkit =
  filter (\t -> t.category == ActionTool) toolkit.tools

-- | Convert toolkit to LLM-facing tool metadata.
toLLMTools :: Toolkit db -> [LLMTool]
toLLMTools toolkit = toLLMTool <$> toolkit.tools

--------------------------------------------------------------------------------
-- Building a Sentinel
--------------------------------------------------------------------------------

-- | Create the runDataTool callback.
--
-- This looks up a tool by name and executes it, returning the observation
-- and produced facts.
makeRunDataTool ::
  Toolkit db ->
  Text ->
  Value ->
  SentinelM db (Either Text (Text, [BaseFact]))
makeRunDataTool toolkit toolName args =
  case lookupTool toolName toolkit of
    Nothing -> pure $ Left $ "Unknown tool: " <> toolName
    Just tool
      | tool.category /= DataTool ->
          pure $ Left $ "Tool " <> toolName <> " is not a data tool"
      | otherwise -> do
          result <- runExceptT (tool.execute args)
          case result of
            Left err -> pure $ Left err
            Right output -> pure $ Right (output.observation, output.producedFacts)

-- | Create a Sentinel from a Toolkit.
--
-- This wires up the guard evaluation and tool execution using the tools
-- in the toolkit.
toolkitSentinel :: Toolkit db -> Sentinel db
toolkitSentinel toolkit =
  Sentinel
    { guardedCall = guardedCallImpl toolkit,
      summarizeFacts = summarizeFactsImpl
    }

-- | Implementation of guardedCall using a Toolkit.
guardedCallImpl :: Toolkit db -> Text -> Value -> SentinelM db SentinelResult
guardedCallImpl toolkit toolName args = do
  -- Log the tool use
  liftIO $ putDispLn (Output.ToolUse toolName (T.Lazy.toStrict $ Aeson.Text.encodeToLazyText args))

  -- Look up the tool
  case lookupTool toolName toolkit of
    Nothing -> do
      let err = "Unknown tool: " <> toolName
      liftIO $ putDispLn (Output.ToolError err)
      pure $ Denied err
    Just tool -> do
      -- Evaluate the guard based on its type
      guardResult <- evaluateToolGuard toolkit tool args

      case guardResult of
        ToolGuardPassed proofs -> do
          liftIO $ putDispLn (Output.GuardPass toolName proofs)
          -- Execute the tool
          result <- runExceptT (tool.execute args)
          case result of
            Left err -> do
              liftIO $ putDispLn (Output.ToolError err)
              pure $ Denied err
            Right output -> do
              -- Add produced facts
              addFacts output.producedFacts
              liftIO $ putDispLn (Output.Observation output.observation)
              pure $ Allowed output.observation
        ToolGuardDenied reason -> do
          liftIO $ putDispLn (Output.GuardDenied toolName reason)
          pure $ Denied reason
        ToolGuardNeedsInput question -> do
          liftIO $ putDispLn (Output.NeedsUserInput toolName question.questionText)
          pure $ AskUser question

-- | Result of evaluating any type of tool guard.
data ToolGuardResult
  = ToolGuardPassed (NonEmpty SolverSuccess) -- ^ Carry the proof(s)
  | ToolGuardDenied Text
  | ToolGuardNeedsInput UserQuestion
  deriving stock (Show, Eq)

-- | Evaluate a tool's guard based on its type.
evaluateToolGuard ::
  Toolkit db ->
  Tool db ->
  Value ->
  SentinelM db ToolGuardResult
evaluateToolGuard toolkit tool args = do
  case tool.guard of
    NoGuard ->
      let noGuardSuccess =
            SolverSuccess
              { bindings = M.empty,
                proof = RuleApplied "no_guard" [],
                reason = "no_guard"
              }
       in pure $ ToolGuardPassed (noGuardSuccess :| [])
    SolverGuardT guardName guardFn -> do
      -- Use the solver-based guard evaluation via runSolver
      sentinelEnv <- ask
      -- Get current stores from SentinelEnv
      baseFactStore <- liftIO $ readIORef sentinelEnv.facts
      ctxStore <- getContextStore
      askStore <- getAskableStore
      -- Create solver environment with bindings and actual stores
      let solverEnv =
            SolverEnv
              { toolBindings = toolkit.toolBindings,
                askables = toolkit.askables,
                contextDecls = toolkit.contextDecls,
                contextStore = ctxStore,
                invokeDataTool = \tName tArgs ->
                  runReaderT (makeRunDataToolForSolver toolkit tName tArgs) sentinelEnv
              }
      let initState = emptySolverState baseFactStore askStore

      -- Wrap the guard function to produce SolverSuccess
      let solverAction = withRule guardName $ do
            innerProof <- guardFn args
            -- Wrap the inner proof with the guard name for hierarchy
            let proof = RuleApplied guardName [innerProof]
            pure
              SolverSuccess
                { bindings = M.empty,
                  proof = proof,
                  reason = guardName
                }

      (result, _finalState) <- liftIO $ runSolver solverEnv initState solverAction
      case result of
        Success successes ->
          pure $ ToolGuardPassed successes
        BlockedOnContext block ->
          -- Convert context block to user question
          let s = block.slot
           in pure
                $ ToolGuardNeedsInput
                  UserQuestion
                    { questionText = "Please specify: " <> s,
                      factDescription = "Context: " <> s
                    }
        BlockedOnAskable block ->
          pure
            $ ToolGuardNeedsInput
              UserQuestion
                { questionText = block.question,
                  factDescription = "Askable: " <> block.predicate
                }
        Failure failures ->
          pure $ ToolGuardDenied (formatSolverFailures failures)

-- | Create a data tool callback for the solver.
--
-- Returns the produced facts directly so the solver can add them to its store.
-- Logs tool invocations for debugging visibility.
makeRunDataToolForSolver ::
  Toolkit db ->
  Text ->
  Value ->
  SentinelM db (Either Text [BaseFact])
makeRunDataToolForSolver toolkit toolName args =
  case lookupTool toolName toolkit of
    Nothing -> pure $ Left $ "Unknown tool: " <> toolName
    Just tool
      | tool.category /= DataTool ->
          pure $ Left $ "Tool " <> toolName <> " is not a data tool"
      | otherwise -> do
          -- Log the tool invocation (solver-initiated, not LLM-initiated)
          liftIO $ putDispLn (Output.QueryExecution toolName (T.Lazy.toStrict $ Aeson.Text.encodeToLazyText args))
          result <- runExceptT (tool.execute args)
          case result of
            Left err -> do
              liftIO $ putDispLn (Output.ToolError err)
              pure $ Left err
            Right output -> do
              -- Log the observation for debugging
              liftIO $ putDispLn (Output.Observation output.observation)
              -- Return the produced facts for the solver to use
              pure $ Right output.producedFacts

-- | Format solver failure paths for display.
formatSolverFailures :: [FailurePath] -> Text
formatSolverFailures [] = "Guard failed with no specific reason"
formatSolverFailures failures =
  T.intercalate "; " [fp.ruleName <> ": " <> fp.reason | fp <- failures]

-- | Summarize current facts for the LLM context.
summarizeFactsImpl :: SentinelM db Text
summarizeFactsImpl = do
  factsRef <- asks (.facts)
  factsDb <- liftIO $ readIORef factsRef
  let allFacts = Facts.allBaseFacts factsDb
  if null allFacts
    then pure "No facts established yet."
    else pure $ T.unlines $ "Known facts:" : fmap (("  - " <>) . renderDocPlain . disp) allFacts

--------------------------------------------------------------------------------
-- Verification Support
--------------------------------------------------------------------------------

-- | Instructions for the LLM to verify claims before stating them.
verificationInstructions :: Toolkit db -> Text
verificationInstructions tk =
  T.unlines
    [ "",
      "VERIFICATION REQUIREMENT:",
      "Before making claims about eligibility to the user, verify them first.",
      "Available verification tools:",
      T.unlines checkGuardDescriptions,
      "Never state eligibility without verification."
    ]
  where
    checkGuardDescriptions =
      [ "- CheckGuard_" <> tool.name <> ": verify " <> guardName
      | (guardName, tool) <- extractVerifiableClaims tk
      ]

-- | Extract verifiable claims from action tools with SolverGuardT guards.
--
-- Returns a list of (guardName, tool) pairs for tools that have solver guards.
extractVerifiableClaims :: Toolkit db -> [(Text, Tool db)]
extractVerifiableClaims tk =
  [ (guardName, tool)
  | tool <- tk.tools,
    tool.category == ActionTool,
    SolverGuardT guardName _ <- [tool.guard]
  ]

-- | Generate a CheckGuard tool for each guarded action tool.
makeCheckGuardTools :: Toolkit db -> [Tool db]
makeCheckGuardTools tk =
  [ makeCheckGuardTool tk guardName tool
  | (guardName, tool) <- extractVerifiableClaims tk
  ]

-- | Create a single CheckGuard tool for a specific action tool.
makeCheckGuardTool :: Toolkit db -> Text -> Tool db -> Tool db
makeCheckGuardTool tk guardName actionTool =
  Tool
    { name = "CheckGuard_" <> actionTool.name,
      description =
        "Verify " <> guardName <> " before stating it to the user. "
          <> "Returns whether the claim is verified, not verified, or needs more information. "
          <> "Uses the same parameters as " <> actionTool.name <> ".",
      params = actionTool.params, -- Reuse exact schema
      category = DataTool,
      guard = NoGuard,
      execute = checkGuardExecute tk guardName
    }

-- | Execute a CheckGuard tool.
checkGuardExecute ::
  Toolkit db ->
  Text ->
  Value ->
  ExceptT Text (SentinelM db) ToolOutput
checkGuardExecute tk guardName args = do
  -- Find the tool with this guard
  tool <- lookup guardName (extractVerifiableClaims tk) ??: ("Unknown guard: " <> guardName)

  -- Run the guard (reusing guard evaluation logic)
  result <- lift $ runGuardOnly tk tool args

  -- Display the eligibility check result to CLI
  liftIO $ putDispLn $ case result of
    ToolGuardPassed proofs -> Output.EligibilityVerified guardName proofs
    ToolGuardDenied reason -> Output.EligibilityDenied guardName reason
    ToolGuardNeedsInput question -> Output.EligibilityNeedsInfo guardName question.questionText

  pure
    ToolOutput
      { observation = formatGuardResult guardName result,
        producedFacts = [] -- CheckGuard is informational only
      }

-- | Run a tool's guard without executing the tool.
--
-- This is used by CheckGuard tools to verify claims without side effects.
runGuardOnly :: Toolkit db -> Tool db -> Value -> SentinelM db ToolGuardResult
runGuardOnly toolkit tool args = evaluateToolGuard toolkit tool args

-- | Format the guard result for display to the LLM.
formatGuardResult :: Text -> ToolGuardResult -> Text
formatGuardResult claimName = \case
  ToolGuardPassed (firstProof :| _) ->
    "VERIFIED: " <> claimName <> " is satisfied. Reason: " <> firstProof.reason
  ToolGuardDenied reason ->
    "NOT VERIFIED: " <> claimName <> " failed. Reason: " <> reason
  ToolGuardNeedsInput question ->
    "NEEDS INFO: " <> question.questionText

-- | Add verification support to a toolkit.
--
-- This adds CheckGuard tools (one per guarded action) and verification
-- instructions to the system prompt.
withVerification :: Toolkit db -> Toolkit db
withVerification tk =
  tk
    { tools = makeCheckGuardTools tk <> tk.tools,
      systemPrompt = tk.systemPrompt <> verificationInstructions tk
    }

