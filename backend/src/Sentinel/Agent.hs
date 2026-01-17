module Sentinel.Agent
  ( AgentConfig (..),
    AgentEnv (..),
    AgentState (..),
    AgentM,
    Message (..),
    runAgent,
    defaultConfig,
    initialMessages,
    runAgentM,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.State.Strict
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import OpenAI.V1 (Methods (..), getClientEnv, makeMethods)
import OpenAI.V1.Chat.Completions
  ( ChatCompletionObject (..),
    Choice (..),
    _CreateChatCompletion,
  )
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.Models (Model)
import OpenAI.V1.Tool qualified as OpenAI
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.Output
import Sentinel.Tool
import Servant.Client (ClientEnv)

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

-- | A message in the conversation (our internal representation).
data Message
  = -- | System message. This is the message which sets the context of the whole
    -- conversation.
    --
    -- TODO: we should move this somewhere else.
    SystemMsg Text
  | -- | User message. This is a message from the (human) end-user.
    UserMsg Text
  | -- | LLM message. First argument is text content, second is tool calls it
    -- wants to make.
    AssistantMsg (Maybe Text) (Maybe (Vector ToolCall.ToolCall))
  | -- | Tool result message with call ID and result. This is the result of a
    -- tool call.
    ToolMsg Text Text
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the agent.
data AgentConfig = AgentConfig
  { clientEnv :: ClientEnv,
    apiKey :: Text,
    model :: Model,
    maxIterations :: Int,
    maxMessages :: Int,
    debug :: Bool
  }

-- | Create default configuration using OpenAI API.
defaultConfig :: Text -> IO AgentConfig
defaultConfig key = do
  clientEnv <- getClientEnv "https://api.openai.com"
  pure
    AgentConfig
      { clientEnv = clientEnv,
        apiKey = key,
        model = "gpt-5-nano-2025-08-07",
        maxIterations = 10,
        maxMessages = 50,
        debug = True
      }

--------------------------------------------------------------------------------
-- AgentM Monad
--------------------------------------------------------------------------------

data AgentEnv db = AgentEnv
  { config :: AgentConfig,
    toolkit :: Toolkit db
  }

data AgentState db = AgentState
  { db :: db,
    messages :: [Message]
  }

type AgentM db = ReaderT (AgentEnv db) (StateT (AgentState db) IO)

runAgentM :: AgentEnv db -> AgentState db -> AgentM db a -> IO (a, AgentState db)
runAgentM env initialState action =
  runStateT (runReaderT action env) initialState

--------------------------------------------------------------------------------
-- Message Conversion
--------------------------------------------------------------------------------

-- | Convert our Message type to the OpenAI Message type.
toOpenAIMessage :: Message -> Chat.Message (Vector Chat.Content)
toOpenAIMessage = \case
  SystemMsg txt ->
    Chat.System
      { content = Vector.singleton (Chat.Text {text = txt}),
        name = Nothing
      }
  UserMsg txt ->
    Chat.User
      { content = Vector.singleton (Chat.Text {text = txt}),
        name = Nothing
      }
  AssistantMsg mContent mToolCalls ->
    Chat.Assistant
      { assistant_content = fmap (Vector.singleton . Chat.Text) mContent,
        name = Nothing,
        refusal = Nothing,
        assistant_audio = Nothing,
        tool_calls = mToolCalls
      }
  ToolMsg callId result ->
    Chat.Tool
      { content = Vector.singleton (Chat.Text {text = result}),
        tool_call_id = callId
      }

--------------------------------------------------------------------------------
-- Context Window Management
--------------------------------------------------------------------------------

-- | Trim history to fit within maxMessages, always keeping the system prompt.
trimHistory :: Int -> [Message] -> [Message]
trimHistory maxMsgs msgs
  | length msgs <= maxMsgs = msgs
  | otherwise =
      case msgs of
        (sys@(SystemMsg _) : rest) ->
          -- Keep system message + last (maxMsgs - 1) messages
          sys : drop (length rest - (maxMsgs - 1)) rest
        _ ->
          -- No system message, just keep last maxMsgs
          drop (length msgs - maxMsgs) msgs

--------------------------------------------------------------------------------
-- LLM API
--------------------------------------------------------------------------------

-- | Call the OpenAI API with native tool support.
callLLM :: [OpenAI.Tool] -> AgentM db (Either Text (Chat.Message Text))
callLLM tools = do
  env <- ask
  st <- get
  let trimmedMsgs = trimHistory env.config.maxMessages st.messages
      methods = makeMethods env.config.clientEnv env.config.apiKey Nothing Nothing
      openAIMessages = Vector.fromList (map toOpenAIMessage trimmedMsgs)
      request =
        _CreateChatCompletion
          { Chat.messages = openAIMessages,
            Chat.model = env.config.model,
            Chat.tools =
              if null tools
                then Nothing
                else Just (Vector.fromList tools),
            Chat.tool_choice =
              if null tools
                then Nothing
                else Just OpenAI.ToolChoiceAuto
          }
  result <- liftIO $ try @SomeException (methods.createChatCompletion request)
  pure $ case result of
    Left err -> Left (T.pack (show err))
    Right ChatCompletionObject {choices} ->
      case Vector.uncons choices of
        Nothing -> Left "No choices returned from API"
        Just (Choice {message}, _) -> Right message

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Extract the input from tool call arguments JSON.
extractToolInput :: Text -> Maybe Text
extractToolInput argsJson =
  case Aeson.decodeStrict (T.encodeUtf8 argsJson) of
    Just (Aeson.Object obj) ->
      case KeyMap.lookup "input" obj of
        Just (Aeson.String input) -> Just input
        _ -> Nothing
    _ -> Nothing

-- | Process a single tool call and return the result message.
processToolCall :: ToolCall.ToolCall -> AgentM db Message
processToolCall (ToolCall.ToolCall_Function {id = callId, function = fn}) = do
  env <- ask
  st <- get
  let toolName = fn.name
      argsJson = fn.arguments
      mInput = extractToolInput argsJson

  liftIO $ putDispLn (ToolUse toolName (fromMaybe argsJson mInput))

  let (result, newDb) = case mInput of
        Nothing -> (ToolError $ "Failed to parse arguments: " <> argsJson, st.db)
        Just input -> env.toolkit.executeTool toolName input st.db

      observation = case result of
        ToolSuccess txt -> txt
        ToolError err -> "ERROR: " <> err

  modify \s -> s {db = newDb}

  liftIO $ putDispLn (Observation observation)

  pure $ ToolMsg callId observation

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

-- | Create initial messages with the system prompt from the toolkit.
initialMessages :: Toolkit db -> [Message]
initialMessages toolkit = [SystemMsg toolkit.systemPrompt]

-- | Run the agent with a user query.
-- Returns the response, the updated database, and the updated conversation history.
--
-- This will /loop/, executing tool calls, etc. It terminates when the LLM
-- returns a "final response" message (i.e. no tool calls). (Or if the maximum
-- number of iterations is reached.)
runAgent :: AgentConfig -> Toolkit db -> db -> [Message] -> Text -> IO (Text, db, [Message])
runAgent config toolkit db history userQuery = do
  putDispLn (AgentStart userQuery)
  let env = AgentEnv {config, toolkit}
      initialState = AgentState {db, messages = history <> [UserMsg userQuery]}
  (response, finalState) <- runAgentM env initialState agentLoop
  pure (response, finalState.db, finalState.messages)

agentLoop :: AgentM db Text
agentLoop = do
  env <- ask
  st <- get
  let iteration = countIterations st.messages
  if iteration > env.config.maxIterations
    then do
      let response = "I apologize, but I was unable to complete your request. Please try again or contact support."
      modify \s -> s {messages = s.messages <> [AssistantMsg (Just response) Nothing]}
      pure response
    else do
      liftIO $ putDispLn (Iteration iteration)
      let openAITools = toOpenAITool <$> env.toolkit.tools
      result <- callLLM openAITools
      case result of
        Left err -> do
          liftIO $ putDispLn (Error err)
          let response = "I encountered an error: " <> err
          modify \s -> s {messages = s.messages <> [AssistantMsg (Just response) Nothing]}
          pure response
        Right msg ->
          handleResponse msg
  where
    -- Count iterations based on assistant messages in history
    countIterations :: [Message] -> Int
    countIterations = length . filter isAssistantMsg
      where
        isAssistantMsg (AssistantMsg _ _) = True
        isAssistantMsg _ = False

-- | Handle an LLM response, processing tool calls or returning final answer.
handleResponse :: Chat.Message Text -> AgentM db Text
handleResponse msg = case msg of
  Chat.Assistant {assistant_content, tool_calls} ->
    case tool_calls of
      Just calls | not (Vector.null calls) -> do
        -- There are tool calls to process
        modify \s -> s {messages = s.messages <> [AssistantMsg assistant_content (Just calls)]}
        toolResults <- traverse processToolCall (Vector.toList calls)
        modify \s -> s {messages = s.messages <> toolResults}
        agentLoop
      _ -> do
        -- No tool calls - this is the final response
        let response = fromMaybe "" assistant_content
        liftIO $ putDispLn (FinalAnswer response)
        modify \s -> s {messages = s.messages <> [AssistantMsg (Just response) Nothing]}
        pure response
  _ -> do
    -- Unexpected message type
    let response = "Unexpected response format from LLM"
    modify \s -> s {messages = s.messages <> [AssistantMsg (Just response) Nothing]}
    pure response
