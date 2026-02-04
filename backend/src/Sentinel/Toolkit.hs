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
    lookupQuery,
    toLLMTools,

    -- * Building a Sentinel from a Toolkit
    toolkitSentinel,

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
import Data.List.NonEmpty qualified as NE
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
    SentinelM,
    SentinelResult (..),
    UserQuestion (..),
    addPendingUserInput,
    clearAllPendingUserInputs,
    emitEvent,
    getAskableStore,
    getContextStore,
    setProofsFound,
  )
import Sentinel.Solver (runSolverFull)
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry (..), formatQuestion, lookupAskable)
import Sentinel.Solver.Combinators (SolverEnv (..), SolverM, SolverState (..), emptySolverState, withRule)
import Sentinel.Solver.ToolBindings (ToolBindingRegistry)
import Sentinel.Solver.Types
  ( BaseFact,
    Proof (..),
    Scalar (..),
    SolverOutcome (..),
    SolverSuccess (..),
    UserInputBlock (..),
    formatSolverOutcomeForLLM,
  )
import Sentinel.Tool (LLMTool, Query (..), Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..), queryToLLMTool, toLLMTool)

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
    -- | First-class queries (solver evaluations reported to LLM)
    queries :: [Query],
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

-- | Look up a query by name.
lookupQuery :: Text -> Toolkit db -> Maybe Query
lookupQuery queryName toolkit =
  find (\q -> q.name == queryName) toolkit.queries

-- | Convert toolkit to LLM-facing tool metadata (tools + queries).
toLLMTools :: Toolkit db -> [LLMTool]
toLLMTools toolkit =
  (toLLMTool <$> toolkit.tools) <> (queryToLLMTool <$> toolkit.queries)

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
--
-- Dispatch order: tool lookup -> query lookup -> unknown tool error.
guardedCallImpl :: Toolkit db -> Text -> Value -> SentinelM db SentinelResult
guardedCallImpl toolkit toolName args = do
  -- Log the tool use
  emitEvent (disp (Output.ToolUse toolName (T.Lazy.toStrict $ Aeson.Text.encodeToLazyText args)))

  -- Look up as tool first, then as query
  case lookupTool toolName toolkit of
    Just tool -> executeToolCall toolkit tool args
    Nothing -> case lookupQuery toolName toolkit of
      Just query -> executeQuery toolkit query args
      Nothing -> do
        let err = "Unknown tool: " <> toolName
        emitEvent (disp (Output.ToolError err))
        pure $ Denied err

-- | Execute a tool call with guard evaluation.
executeToolCall :: Toolkit db -> Tool db -> Value -> SentinelM db SentinelResult
executeToolCall toolkit tool args = do
  -- Evaluate the guard based on its type
  (guardResult, guardOutcome) <- evaluateToolGuard toolkit tool args

  case guardResult of
    ToolGuardPassed proofs -> do
      unless (isNoGuard tool.guard) $
        emitEvent (disp (Output.GuardPass tool.name proofs))
      -- Execute the tool
      result <- runExceptT (tool.execute args)
      case result of
        Left err -> do
          emitEvent (disp (Output.ToolError err))
          pure $ Denied err
        Right output -> do
          -- Add produced facts
          addFacts output.producedFacts
          -- Process tool's solver outcome (register blocks, console display)
          for_ output.solverOutcome \toolOutcome -> do
            -- Track whether proofs were found alongside blocks
            setProofsFound (not (null toolOutcome.successes))
            -- Register ALL blocked items from the tool's solver outcome
            for_ toolOutcome.blocked \block ->
              addPendingUserInput
                block.inputType
                block.name
                block.arguments
                block.question
                block.candidates
            -- Console display
            emitEvent ("" <> line <> indent 2 (disp toolOutcome))
          -- Combine observation text
          let guardText = case guardOutcome of
                Just go | not (isNoGuard tool.guard) -> formatSolverOutcomeForLLM go
                _ -> ""
              toolOutcomeText = maybe "" formatSolverOutcomeForLLM output.solverOutcome
              toolObsText = output.observation
              combined = T.intercalate "\n" $ filter (not . T.null)
                [guardText, toolOutcomeText, toolObsText]
          when (not (T.null output.observation)) $
            emitEvent (disp (Output.Observation output.observation))
          pure $ Allowed combined
    ToolGuardDenied _reason -> do
      let deniedText = maybe "Guard failed." formatSolverOutcomeForLLM guardOutcome
      emitEvent (disp (Output.GuardDenied tool.name deniedText))
      pure $ Denied deniedText
    ToolGuardNeedsInput question -> do
      let blockedText = maybe "" formatSolverOutcomeForLLM guardOutcome
      emitEvent (disp (Output.NeedsUserInput tool.name question.questionText))
      pure $ AskUser question blockedText

-- | Check if a tool guard is NoGuard.
isNoGuard :: ToolGuard -> Bool
isNoGuard NoGuard = True
isNoGuard _ = False

-- | Result of evaluating any type of tool guard.
data ToolGuardResult
  = -- | Carry the proof(s)
    ToolGuardPassed (NonEmpty SolverSuccess)
  | ToolGuardDenied Text
  | ToolGuardNeedsInput UserQuestion
  deriving stock (Show, Eq)

-- | Run the solver within the SentinelM monad.
--
-- Shared helper that:
-- - Reads stores via proper helpers
-- - Constructs SolverEnv with toolkit's bindings/askables/contextDecls
-- - Runs runSolverFull
-- - Persists discovered facts back
-- - Sets proofsFound flag
-- - Registers all blocked items
-- - Returns SolverOutcome
runSolverInSentinel ::
  Toolkit db ->
  Text ->
  (Value -> SolverM SolverSuccess) ->
  Value ->
  SentinelM db SolverOutcome
runSolverInSentinel toolkit solverLabel solverAction args = do
  sentinelEnv <- ask
  -- Get current stores via accessors
  baseFactStore <- getFactStore
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

  (outcome, finalState) <- liftIO $ runSolverFull solverLabel solverEnv initState (solverAction args)
  -- Persist any facts discovered during solver run back to SentinelM
  addFacts (Facts.allBaseFacts finalState.baseFactStore)

  -- Track whether proofs were found alongside blocks
  setProofsFound (not (null outcome.successes))

  -- Register ALL blocked items from the outcome
  for_ outcome.blocked \block ->
    addPendingUserInput
      block.inputType
      block.name
      block.arguments
      block.question
      block.candidates

  pure outcome

-- | Evaluate a tool's guard based on its type.
--
-- Returns the guard result and, for solver-based guards, the full 'SolverOutcome'.
evaluateToolGuard ::
  Toolkit db ->
  Tool db ->
  Value ->
  SentinelM db (ToolGuardResult, Maybe SolverOutcome)
evaluateToolGuard toolkit tool args = do
  case tool.guard of
    NoGuard ->
      let noGuardSuccess =
            SolverSuccess
              { bindings = M.empty,
                proof = RuleApplied "no_guard" [],
                reason = "no_guard"
              }
       in pure (ToolGuardPassed (noGuardSuccess :| []), Nothing)
    SolverGuardT guardName guardFn -> do
      let solverAction guardArgs = withRule guardName $ do
            proof <- guardFn guardArgs
            pure
              SolverSuccess
                { bindings = M.empty,
                  proof = proof,
                  reason = guardName
                }

      outcome <- runSolverInSentinel toolkit guardName solverAction args

      case NE.nonEmpty outcome.successes of
        Just successes ->
          pure (ToolGuardPassed successes, Just outcome)
        Nothing ->
          case outcome.blocked of
            (block : _) ->
              -- Primary block for the AskUser response
              pure
                ( ToolGuardNeedsInput
                    UserQuestion
                      { inputType = block.inputType,
                        inputName = block.name,
                        arguments = block.arguments,
                        questionText = block.question,
                        candidates = block.candidates
                      },
                  Just outcome
                )
            [] ->
              pure (ToolGuardDenied (formatSolverOutcomeForLLM outcome), Just outcome)

-- | Execute a query: run the solver and report results to the LLM.
--
-- Queries give a fresh complete picture, so pending user inputs are cleared first.
executeQuery :: Toolkit db -> Query -> Value -> SentinelM db SentinelResult
executeQuery toolkit query args = do
  -- Clear old pending inputs — query gives a fresh complete picture
  clearAllPendingUserInputs

  let solverAction queryArgs = withRule query.name $ do
        proof <- query.goal queryArgs
        pure
          SolverSuccess
            { bindings = M.empty,
              proof = proof,
              reason = query.name
            }

  outcome <- runSolverInSentinel toolkit query.name solverAction args

  -- Console display
  emitEvent ("" <> line <> indent 2 (disp outcome))

  pure $ Allowed (formatSolverOutcomeForLLM outcome)

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
          emitEvent (disp (Output.QueryExecution toolName (T.Lazy.toStrict $ Aeson.Text.encodeToLazyText args)))
          result <- runExceptT (tool.execute args)
          case result of
            Left err -> do
              emitEvent (disp (Output.ToolError err))
              pure $ Left err
            Right output -> do
              -- Log the observation for debugging
              emitEvent (disp (Output.Observation output.observation))
              -- Return the produced facts for the solver to use
              pure $ Right output.producedFacts

-- | Summarize current facts for the LLM context.
summarizeFactsImpl :: SentinelM db Text
summarizeFactsImpl = do
  factsDb <- getFactStore
  let allFacts = Facts.allBaseFacts factsDb
  if null allFacts
    then pure "No facts established yet."
    else pure $ T.unlines $ "Known facts:" : fmap (("  - " <>) . renderDocPlain . disp) allFacts

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
                  solverOutcome = Nothing
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
                  solverOutcome = Nothing
                }
        }
