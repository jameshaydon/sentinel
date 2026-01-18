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

import Data.Aeson (Value)
import Data.Aeson.Text qualified as Aeson.Text
import Data.IORef (readIORef)
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Pre
import Sentinel.Facts qualified as Facts
import Sentinel.Guard (GuardEnv (..), GuardResult (..), evaluateGuard)
import Sentinel.Guard qualified as Guard
import Sentinel.Output qualified as Output
import Sentinel.Sentinel (Sentinel (..), SentinelEnv (..), SentinelM, SentinelResult (..), UserQuestion (..), addFacts)
import Sentinel.Tool (LLMTool, Tool (..), ToolCategory (..), ToolOutput (..), toLLMTool)

--------------------------------------------------------------------------------
-- Toolkit Type
--------------------------------------------------------------------------------

-- | A toolkit bundles tools with a system prompt.
--
-- Type parameters:
-- - @db@: The database type (e.g., AirlineDB)
-- - @fact@: The fact type (e.g., AirCanada.Fact)
data Toolkit db fact = Toolkit
  { -- | All tools in this toolkit
    tools :: [Tool db fact],
    -- | System prompt for the LLM
    systemPrompt :: Text
  }

--------------------------------------------------------------------------------
-- Toolkit Operations
--------------------------------------------------------------------------------

-- | Look up a tool by name.
lookupTool :: Text -> Toolkit db fact -> Maybe (Tool db fact)
lookupTool toolName toolkit =
  find (\t -> t.name == toolName) toolkit.tools

-- | Get all data tools (can be auto-invoked by guards).
dataTools :: Toolkit db fact -> [Tool db fact]
dataTools toolkit =
  filter (\t -> t.category == DataTool) toolkit.tools

-- | Get all action tools (require explicit LLM invocation).
actionTools :: Toolkit db fact -> [Tool db fact]
actionTools toolkit =
  filter (\t -> t.category == ActionTool) toolkit.tools

-- | Convert toolkit to LLM-facing tool metadata.
toLLMTools :: Toolkit db fact -> [LLMTool]
toLLMTools toolkit = toLLMTool <$> toolkit.tools

--------------------------------------------------------------------------------
-- Building a Sentinel
--------------------------------------------------------------------------------

-- | Create the runDataTool callback for GuardEnv.
--
-- This looks up a tool by name and executes it, returning the observation
-- and produced facts.
makeRunDataTool ::
  Toolkit db fact ->
  Text ->
  Value ->
  SentinelM db fact (Either Text (Text, [fact]))
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
toolkitSentinel :: (Ord fact, Show fact) => Toolkit db fact -> Sentinel db fact
toolkitSentinel toolkit =
  Sentinel
    { guardedCall = guardedCallImpl toolkit,
      summarizeFacts = summarizeFactsImpl
    }

-- | Implementation of guardedCall using a Toolkit.
guardedCallImpl :: (Ord fact) => Toolkit db fact -> Text -> Value -> SentinelM db fact SentinelResult
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
      -- Build the guard environment
      sentinelEnv <- ask
      let guardEnv =
            GuardEnv
              { sentinelEnv = sentinelEnv,
                runDataTool = makeRunDataTool toolkit
              }

      -- Evaluate the guard
      guardResult <- liftIO $ evaluateGuard guardEnv (tool.guard args)

      case guardResult of
        GuardAllowed -> do
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
        Guard.GuardDenied failure -> do
          let reason = formatGuardFailure failure
          liftIO $ putDispLn (Output.GuardDenied toolName reason)
          pure $ Denied reason
        Guard.NeedsUserInput question -> do
          liftIO $ putDispLn (Output.NeedsUserInput toolName question.questionText)
          pure $ AskUser question

-- | Format a guard failure for display.
formatGuardFailure :: Guard.GuardFailure -> Text
formatGuardFailure failure =
  case failure.reasons of
    [] -> "Guard failed with no specific reason"
    reasons -> T.intercalate "; " (formatReason <$> reasons)
  where
    formatReason :: Guard.FailureReason -> Text
    formatReason = \case
      Guard.MissingFact desc -> "Missing fact: " <> desc
      Guard.ForbiddenFact desc -> "Forbidden: " <> desc
      Guard.ExplicitDenial reason -> reason
      Guard.QueryFailed tool err -> "Query " <> tool <> " failed: " <> err

-- | Summarize current facts for the LLM context.
summarizeFactsImpl :: (Show fact) => SentinelM db fact Text
summarizeFactsImpl = do
  factsRef <- asks (.facts)
  factsDb <- liftIO $ readIORef factsRef
  let allFacts = Facts.allFacts factsDb
  if null allFacts
    then pure "No facts established yet."
    else pure $ T.unlines $ "Known facts:" : fmap (("  - " <>) . T.pack . show) allFacts
