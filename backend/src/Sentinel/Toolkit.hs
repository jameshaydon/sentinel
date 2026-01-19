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
  )
where

import Data.Aeson (Value, toJSON)
import Data.Aeson.Text qualified as Aeson.Text
import Data.IORef (readIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Pre
import Sentinel.Context (emptyContextDecls)
import Sentinel.Facts qualified as Facts
import Sentinel.Output qualified as Output
import Sentinel.Sentinel (Sentinel (..), SentinelEnv (..), SentinelM, SentinelResult (..), UserQuestion (..), addFacts, getAskableStore, getContextStore)
import Sentinel.Solver (runSolver)
import Sentinel.Solver.Askable (emptyAskableRegistry)
import Sentinel.Solver.Combinators (SolverEnv (..), emptySolverState, withRule)
import Sentinel.Solver.ToolBindings (ToolBindingRegistry)
import Sentinel.Solver.Types
  ( AskableBlock (..),
    BaseFact,
    ContextBlock (..),
    FailurePath (..),
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
    toolBindings :: ToolBindingRegistry
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
        ToolGuardPassed -> do
          liftIO $ putDispLn (Output.GuardPass toolName)
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
  = ToolGuardPassed
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
      pure ToolGuardPassed
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
                askables = emptyAskableRegistry,
                contextDecls = emptyContextDecls,
                contextStore = ctxStore,
                invokeDataTool = \tName tArgs ->
                  runReaderT (makeRunDataToolForSolver toolkit tName tArgs) sentinelEnv
              }
      let initState = emptySolverState baseFactStore askStore

      -- Wrap the guard function to produce SolverSuccess
      let solverAction = withRule guardName $ do
            proof <- guardFn args
            pure
              SolverSuccess
                { bindings = M.empty,
                  proof = proof,
                  reason = guardName
                }

      (result, _finalState) <- liftIO $ runSolver solverEnv initState solverAction
      case result of
        Success _ ->
          pure ToolGuardPassed
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
makeRunDataToolForSolver ::
  Toolkit db ->
  Text ->
  Value ->
  SentinelM db (Either Text Value)
makeRunDataToolForSolver toolkit toolName args =
  case lookupTool toolName toolkit of
    Nothing -> pure $ Left $ "Unknown tool: " <> toolName
    Just tool
      | tool.category /= DataTool ->
          pure $ Left $ "Tool " <> toolName <> " is not a data tool"
      | otherwise -> do
          result <- runExceptT (tool.execute args)
          case result of
            Left err -> pure $ Left err
            Right output -> do
              -- Return the observation as JSON (simplified for now)
              pure $ Right (toJSON output.observation)

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
