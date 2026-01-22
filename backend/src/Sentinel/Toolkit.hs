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
    toLLMTools,

    -- * Building a Sentinel from a Toolkit
    toolkitSentinel,

    -- * Verification Support
    withVerification,
    extractVerifiableClaims,

    -- * Dynamic Ask Tools
    makeDynamicAskTools,

    -- * Re-exports for lookups
    ContextDecl,
    lookupContextDecl,
    lookupAskable,
  )
where

import Data.Aeson (Value)
import Data.Aeson.Text qualified as Aeson.Text
import Data.IORef (readIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Pre
import Sentinel.Context (AskableSpec (..), ContextDecl (..), ContextDecls (..), lookupContextDecl)
import Sentinel.Facts (HasFactStore (..))
import Sentinel.Facts qualified as Facts
import Sentinel.Output qualified as Output
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel
  ( Sentinel (..),
    SentinelEnv (..),
    SentinelM,
    SentinelResult (..),
    UserQuestion (..),
    addPendingUserInput,
    getAskableStore,
    getContextStore,
  )
import Sentinel.Solver (runSolver)
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry (..), formatQuestion, lookupAskable)
import Sentinel.Solver.Combinators (SolverEnv (..), SolverState (..), emptySolverState, withRule)
import Sentinel.Solver.ToolBindings (ToolBindingRegistry)
import Sentinel.Solver.Types
  ( BaseFact,
    FailurePath (..),
    Proof (..),
    Scalar (..),
    SolverResult (..),
    SolverSuccess (..),
    UserInputBlock (..),
    UserInputType (..),
  )
import Sentinel.Tool (BlockedItem (..), LLMTool, Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..), toLLMTool)

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

-- | Convert toolkit to LLM-facing tool metadata.
toLLMTools :: Toolkit db -> [LLMTool]
toLLMTools toolkit = toLLMTool <$> toolkit.tools

--------------------------------------------------------------------------------
-- Building a Sentinel
--------------------------------------------------------------------------------

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
  = -- | Carry the proof(s)
    ToolGuardPassed (NonEmpty SolverSuccess)
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

      -- Run the guard function to produce SolverSuccess
      let solverAction = withRule guardName $ do
            proof <- guardFn args
            pure
              SolverSuccess
                { bindings = M.empty,
                  proof = proof,
                  reason = guardName
                }

      (result, finalState) <- liftIO $ runSolver solverEnv initState solverAction
      -- Persist any facts discovered during solver run back to SentinelM
      addFacts (Facts.allBaseFacts finalState.baseFactStore)
      case result of
        Success successes ->
          pure $ ToolGuardPassed successes
        BlockedOnUserInput block -> do
          -- Register the pending user input so assessment runs on next turn
          addPendingUserInput
            block.inputType
            block.name
            block.arguments
            block.question
            [] -- No candidates provided from solver
            -- Convert block to user question
          pure
            $ ToolGuardNeedsInput
              UserQuestion
                { inputType = block.inputType,
                  inputName = block.name,
                  arguments = block.arguments,
                  questionText = block.question,
                  candidates = []
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
verificationInstructions :: Text
verificationInstructions =
  T.unlines
    [ "",
      "VERIFICATION REQUIREMENT:",
      "Before making claims about eligibility to the user, use the CheckGuard tools to verify them first.",
      "Never state eligibility without verification."
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
        "Check eligibility for "
          <> actionTool.name
          <> " before attempting to execute it. "
          <> "Returns whether eligible, not eligible, or needs more information. "
          <> "Tool context: "
          <> actionTool.description,
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

  -- Extract blocked items from NeedsInput result
  let blocked = case result of
        ToolGuardNeedsInput question ->
          case question.inputType of
            AskableInput -> [BlockedAskable question.inputName question.arguments]
            ContextInput -> [BlockedContext question.inputName]
        _ -> []

  pure
    ToolOutput
      { observation = formatGuardResult guardName result,
        producedFacts = [], -- CheckGuard is informational only
        triggerSideSession = Nothing,
        blockedOn = blocked
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
    "BLOCKED on " <> question.inputName

-- | Add verification support to a toolkit.
--
-- This adds CheckGuard tools (one per guarded action) and verification
-- instructions to the system prompt.
--
-- Note: Ask tools for context variables and askables are now generated
-- dynamically by the agent when blocked, not added upfront.
withVerification :: Toolkit db -> Toolkit db
withVerification tk =
  tk
    { tools =
        makeCheckGuardTools tk
          <> tk.tools,
      systemPrompt = tk.systemPrompt <> verificationInstructions
    }

--------------------------------------------------------------------------------
-- Dynamic Ask Tools
--------------------------------------------------------------------------------

-- | Generate dynamic Ask tools based on currently blocked items.
--
-- This creates Ask_<name> tools for each blocked context variable and
-- askable predicate. These tools are only available when the corresponding
-- item is blocked, and calling them triggers a side conversation session.
makeDynamicAskTools ::
  Toolkit db ->
  [Text] -> -- Blocked context variable names
  [(Text, [Scalar])] -> -- Blocked askables (predicate name, arguments)
  [Tool db]
makeDynamicAskTools tk blockedCtx blockedAskables =
  -- Context variable Ask tools
  [ makeDynamicAskContextTool decl
  | ctxName <- blockedCtx,
    Just decl <- [lookupContextDecl ctxName tk.contextDecls]
  ]
    ++
    -- Askable Ask tools (one per blocked askable with its args)
    [ makeDynamicAskAskableTool decl args
    | (predName, args) <- blockedAskables,
      Just decl <- [lookupAskable predName tk.askables]
    ]

-- | Create a dynamic Ask tool for a blocked context variable.
--
-- Unlike the upfront Ask tools, this is only created when the context
-- variable is actually blocked. The tool triggers a side session when called.
makeDynamicAskContextTool :: ContextDecl -> Tool db
makeDynamicAskContextTool decl =
  let questionText = maybe ("Please specify " <> decl.name) (.questionTemplate) decl.askable
   in Tool
        { name = "Ask_" <> decl.name,
          description =
            "Ask the user: "
              <> questionText
              <> " (This context variable is currently blocking the guard. "
              <> "Calling this will prompt the user immediately.)",
          params = Schema.objectSchema [] [],
          category = DataTool,
          guard = NoGuard,
          -- The actual execution is handled by the agent, not here
          execute = \_ ->
            pure
              ToolOutput
                { observation = "Side session triggered for " <> decl.name,
                  producedFacts = [],
                  triggerSideSession = Nothing, -- Handled by agent
                  blockedOn = []
                }
        }

-- | Create a dynamic Ask tool for a blocked askable predicate.
--
-- The arguments are baked into the tool since they were determined when
-- the guard blocked. The tool triggers a side session when called.
makeDynamicAskAskableTool :: AskableDecl -> [Scalar] -> Tool db
makeDynamicAskAskableTool decl args =
  let formattedQuestion = formatQuestion decl.questionTemplate args
   in Tool
        { name = "Ask_" <> decl.predicate,
          description =
            "Ask the user: "
              <> formattedQuestion
              <> " (This askable is currently blocking the guard. "
              <> "Calling this will prompt the user immediately.)",
          params = Schema.objectSchema [] [],
          category = DataTool,
          guard = NoGuard,
          -- The actual execution is handled by the agent, not here
          execute = \_ ->
            pure
              ToolOutput
                { observation = "Side session triggered for " <> decl.predicate,
                  producedFacts = [],
                  triggerSideSession = Nothing, -- Handled by agent
                  blockedOn = []
                }
        }
