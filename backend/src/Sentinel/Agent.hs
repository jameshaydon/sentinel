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
import Data.Text.Encoding qualified as T
import Data.Vector qualified as Vector
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.LLM (Message (..))
import Sentinel.LLM qualified as LLM
import Sentinel.Output qualified as Output
import Sentinel.Tool qualified as Tool

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

data AgentState db = AgentState
  { db :: db,
    -- | Messages stored in reverse order (newest first) for efficient prepending.
    messages :: [Message],
    turnCount :: !Int,
    iterationCount :: !Int
  }
  deriving stock (Generic)

type AgentM db = ReaderT (AgentEnv db) (StateT (AgentState db) IO)

data AgentEnv db = AgentEnv
  { config :: AgentConfig,
    toolkit :: Tool.Toolkit (AgentM db),
    systemPrompt :: Text
  }

runAgentM :: AgentEnv db -> AgentState db -> AgentM db a -> IO (a, AgentState db)
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
callLLM tools = do
  env <- ask
  msgs <- use #messages
  let trimmedMsgs = trimHistory env.config.maxMessages msgs
      -- Reverse to get chronological order for the API
      chronologicalMsgs = reverse trimmedMsgs
  liftIO $ LLM.callChatCompletion env.config.llmConfig env.systemPrompt chronologicalMsgs tools

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Process a single tool call and return the result message.
processToolCall :: ToolCall.ToolCall -> AgentM db Message
processToolCall (ToolCall.ToolCall_Function {id = callId, function = fn}) = do
  env <- ask
  let toolName = fn.name
      argsJson = fn.arguments

  liftIO $ putDispLn (Output.ToolUse toolName argsJson)

  -- Decode the raw JSON string from OpenAI into a Value
  let mArgs = Aeson.decodeStrict (T.encodeUtf8 argsJson) :: Maybe Aeson.Value

  result <- case mArgs of
    Nothing -> pure $ Left $ "Could not parse arguments JSON: " <> argsJson
    Just args -> do
      -- Find the tool in the list
      let foundTool = find (\t -> t.toolName == toolName) env.toolkit.tools
      case foundTool of
        Nothing -> pure $ Left $ "Tool not found: " <> toolName
        Just tool -> do
          -- Check the guard first
          guardResult <- tool.toolGuard args
          case guardResult of
            Just reason -> do
              liftIO $ putDispLn (Output.GuardDenied toolName reason)
              pure $ Left $ "Guard denied: " <> reason
            Nothing -> do
              liftIO $ putDispLn (Output.GuardPass toolName)
              -- Execute the tool action (which can modify DB state via 'modify')
              runExceptT (tool.toolAction args)

  let resultText = either ("ERROR: " <>) (\x -> x) result
  liftIO $ case result of
    Left err -> putDispLn (Output.ToolError err)
    Right obs -> putDispLn (Output.Observation obs)

  pure $ ToolMsg callId resultText

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

runAgent ::
  AgentConfig ->
  Tool.Toolkit (AgentM db) ->
  db ->
  [Message] ->
  Int ->
  Text ->
  IO (Text, db, [Message], Int)
runAgent config toolkit db history turnCount userQuery = do
  let newTurnCount = turnCount + 1
  putDispLn (Output.TurnStart newTurnCount userQuery)
  let env =
        AgentEnv
          { config,
            toolkit,
            systemPrompt = toolkit.systemPrompt
          }
      initialState =
        AgentState
          { db,
            messages = UserMsg userQuery : history,
            turnCount = newTurnCount,
            iterationCount = 0
          }
  (response, finalState) <- runAgentM env initialState agentLoop
  pure (response, finalState.db, finalState.messages, finalState.turnCount)

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
      let openAITools = Tool.toOpenAITool <$> env.toolkit.tools
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
