module Sentinel.Agent
  ( AgentConfig (..),
    AgentEnv (..),
    AgentState (..),
    AgentM,
    Message (..),
    runAgent,
    defaultConfig,
    runAgentM,
  )
where

import Control.Monad.State.Strict
import Data.Aeson qualified as Aeson
import Data.Text.Encoding qualified as T.Encoding
import Data.Vector qualified as Vector
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.LLM (Message (..))
import Sentinel.LLM qualified as LLM
import Sentinel.Output qualified as Output
import Sentinel.Sentinel (Sentinel, SentinelEnv, SentinelResult (..))
import Sentinel.Sentinel qualified as Sentinel
import Sentinel.Tool qualified as Tool

-- Note: fact type parameter has been removed from Sentinel types.
-- All facts are now stored as BaseFact in BaseFactStore.

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the agent.
data AgentConfig = AgentConfig
  { llmConfig :: LLM.LLMConfig,
    maxIterations :: Int,
    maxMessages :: Int
  }

-- | Create default configuration using OpenAI API.
defaultConfig :: Text -> IO AgentConfig
defaultConfig key = do
  llmConfig <- LLM.defaultConfig key
  pure
    AgentConfig
      { llmConfig = llmConfig,
        maxIterations = 10,
        maxMessages = 50
      }

--------------------------------------------------------------------------------
-- AgentM Monad
--------------------------------------------------------------------------------

-- | Agent state - only contains conversation-related state.
-- Database and facts are now managed by Sentinel via SentinelEnv.
data AgentState = AgentState
  { -- | Messages stored in reverse order (newest first) for efficient prepending.
    messages :: [Message],
    turnCount :: !Int,
    iterationCount :: !Int
  }
  deriving stock (Generic)

type AgentM db = ReaderT (AgentEnv db) (StateT AgentState IO)

data AgentEnv db = AgentEnv
  { config :: AgentConfig,
    -- | Tool definitions for LLM (schemas and descriptions).
    tools :: [Tool.LLMTool],
    -- | System prompt for the LLM.
    systemPrompt :: Text,
    -- | Sentinel for guarded tool calls.
    sentinel :: Sentinel db,
    -- | Sentinel environment (shared state for db and facts).
    sentinelEnv :: SentinelEnv db
  }

runAgentM :: AgentEnv db -> AgentState -> AgentM db a -> IO (a, AgentState)
runAgentM env initialState action =
  runStateT (runReaderT action env) initialState

--------------------------------------------------------------------------------
-- Context Window Management
--------------------------------------------------------------------------------

-- | Trim history to keep only the most recent messages.
-- Since messages are stored in reverse order, we just take from the front.
trimHistory :: Int -> [Message] -> [Message]
trimHistory maxMsgs = take maxMsgs

--------------------------------------------------------------------------------
-- LLM API
--------------------------------------------------------------------------------

callLLM :: [LLM.Tool] -> AgentM db (Either Text (Chat.Message Text))
callLLM llmTools = do
  env <- ask
  msgs <- use #messages
  let trimmedMsgs = trimHistory env.config.maxMessages msgs
      -- Reverse to get chronological order for the API
      chronologicalMsgs = reverse trimmedMsgs
  liftIO $ LLM.callChatCompletion env.config.llmConfig env.systemPrompt chronologicalMsgs llmTools

--------------------------------------------------------------------------------
-- Tool Execution (via Sentinel)
--------------------------------------------------------------------------------

-- | Process a single tool call via Sentinel and return the result message.
processToolCall :: ToolCall.ToolCall -> AgentM db Message
processToolCall (ToolCall.ToolCall_Function {id = callId, function = fn}) = do
  env <- ask
  let toolName = fn.name
      argsJson = fn.arguments

  -- Decode the raw JSON string from OpenAI into a Value
  let mArgs = Aeson.decodeStrict (T.Encoding.encodeUtf8 argsJson) :: Maybe Aeson.Value

  case mArgs of
    Nothing -> do
      let err = "Could not parse arguments JSON: " <> argsJson
      liftIO $ putDispLn (Output.ToolError err)
      pure $ ToolMsg callId ("ERROR: " <> err)
    Just args -> do
      -- Call Sentinel to guard and execute the tool
      result <- liftIO $ Sentinel.runSentinelM env.sentinelEnv (env.sentinel.guardedCall toolName args)
      case result of
        Allowed obs -> do
          pure $ ToolMsg callId obs
        Denied reason -> do
          pure $ ToolMsg callId ("ERROR: " <> reason)
        AskUser question -> do
          -- Return the question to the LLM so it can ask the user.
          -- The LLM should:
          -- 1. Ask the user the question
          -- 2. Based on the response, call EstablishContext or EstablishAskable
          -- 3. Retry the original tool call
          pure $
            ToolMsg callId $
              "ACTION REQUIRED: I need more information before I can proceed.\n"
                <> "Question: "
                <> question.questionText
                <> "\n"
                <> "Context: "
                <> question.factDescription
                <> "\n"
                <> "Please ask the user this question, then use the appropriate tool "
                <> "(EstablishContext or EstablishAskable) to record their response."

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

-- | Run the agent with a user query.
--
-- Takes:
-- - Agent configuration
-- - Tool definitions for the LLM (LLMTool metadata)
-- - System prompt
-- - Sentinel and its environment (for guarded tool calls)
-- - Message history
-- - Current turn count
-- - User's query
--
-- Returns:
-- - Response text
-- - Updated message history
-- - Updated turn count
runAgent ::
  AgentConfig ->
  [Tool.LLMTool] ->
  Text ->
  Sentinel db ->
  SentinelEnv db ->
  [Message] ->
  Int ->
  Text ->
  IO (Text, [Message], Int)
runAgent config tools systemPrompt sentinel sentinelEnv history turnCount userQuery = do
  let newTurnCount = turnCount + 1
  putDispLn (Output.TurnStart newTurnCount userQuery)
  let env =
        AgentEnv
          { config,
            tools,
            systemPrompt,
            sentinel,
            sentinelEnv
          }
      initialState =
        AgentState
          { messages = UserMsg userQuery : history,
            turnCount = newTurnCount,
            iterationCount = 0
          }
  (response, finalState) <- runAgentM env initialState agentLoop
  pure (response, finalState.messages, finalState.turnCount)

agentLoop :: AgentM db Text
agentLoop = do
  env <- ask
  iteration <- use #iterationCount
  if iteration > env.config.maxIterations
    then do
      let response = "I apologize, but I was unable to complete your request. Please try again or contact support."
      #messages %= (AssistantMsg (Just response) Nothing :)
      pure response
    else do
      liftIO $ putDispLn (Output.Iteration iteration)
      #iterationCount += 1
      let openAITools = Tool.toOpenAITool <$> env.tools
      result <- callLLM openAITools
      case result of
        Left err -> do
          liftIO $ putDispLn (Output.Error err)
          let response = "I encountered an error: " <> err
          #messages %= (AssistantMsg (Just response) Nothing :)
          pure response
        Right msg ->
          handleResponse msg

handleResponse :: Chat.Message Text -> AgentM db Text
handleResponse msg = case msg of
  Chat.Assistant {assistant_content, tool_calls} ->
    case tool_calls of
      Just calls | not (Vector.null calls) -> do
        #messages %= (AssistantMsg assistant_content (Just calls) :)
        toolResults <- traverse processToolCall (Vector.toList calls)
        -- Prepend tool results in reverse order so they end up in correct order
        #messages %= (reverse toolResults ++)
        agentLoop
      _ -> do
        let response = fromMaybe "" assistant_content
        liftIO $ putDispLn (Output.FinalAnswer response)
        #messages %= (AssistantMsg (Just response) Nothing :)
        pure response
  _ -> do
    let response = "Unexpected response format from LLM"
    #messages %= (AssistantMsg (Just response) Nothing :)
    pure response
