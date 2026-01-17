module Sentinel.Agent
  ( AgentConfig (..),
    Message (..),
    Role (..),
    runAgent,
    defaultConfig,
    initialMessages,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import OpenAI.V1 (Methods (..), getClientEnv, makeMethods)
import OpenAI.V1.Chat.Completions
  ( ChatCompletionObject (..),
    Choice (..),
    _CreateChatCompletion,
    messageToContent,
  )
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.Models (Model)
import Pre
import Sentinel.Output
import Sentinel.Tool
import Servant.Client (ClientEnv)

-- | Role of a message in the conversation.
data Role = System | User | Assistant
  deriving stock (Show, Eq)

instance ToJSON Role where
  toJSON = \case
    System -> Aeson.String "system"
    User -> Aeson.String "user"
    Assistant -> Aeson.String "assistant"

-- | A message in the conversation.
data Message = Message
  { role :: Role,
    content :: Text
  }
  deriving stock (Show, Eq)

instance ToJSON Message where
  toJSON msg =
    Aeson.object
      [ "role" Aeson..= msg.role,
        "content" Aeson..= msg.content
      ]

-- | Configuration for the agent.
data AgentConfig = AgentConfig
  { clientEnv :: ClientEnv,
    apiKey :: Text,
    model :: Model,
    maxIterations :: Int,
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
        model = "gpt-4-turbo",
        maxIterations = 10,
        debug = True
      }

-- | Parse the LLM response to extract components of the ReAct pattern.
data ParsedResponse
  = ActionRequest Text Text Text -- thought, tool name, input
  | ParsedFinalAnswer Text Text -- thought, answer
  | InvalidFormat Text
  deriving stock (Show)

parseResponse :: Text -> ParsedResponse
parseResponse response =
  let lines_ = T.lines response
      findLine prefix = listToMaybe $ mapMaybe (T.stripPrefix prefix) lines_
      -- Extract thought: find "Thought:" line content
      thought = maybe "" T.strip (findLine "Thought: ")
   in case T.breakOn "Final Answer: " response of
        (_, "") ->
          -- "Final Answer: " not found, check for action/input
          case (findLine "Action: ", findLine "Action Input: ") of
            (Just action, Just input) ->
              ActionRequest thought (T.strip action) (T.strip input)
            _ -> InvalidFormat response
        (_, rest) ->
          -- Found "Final Answer: ", extract everything after it
          let answer = T.drop (T.length "Final Answer: ") rest
           in ParsedFinalAnswer thought (T.strip answer)

-- | Convert our Message type to the openai Message type.
toOpenAIMessage :: Message -> Chat.Message (Vector Chat.Content)
toOpenAIMessage msg = case msg.role of
  System -> Chat.System {content = Vector.singleton (Chat.Text {text = msg.content}), name = Nothing}
  User -> Chat.User {content = Vector.singleton (Chat.Text {text = msg.content}), name = Nothing}
  Assistant ->
    Chat.Assistant
      { assistant_content = Just (Vector.singleton (Chat.Text {text = msg.content})),
        name = Nothing,
        refusal = Nothing,
        assistant_audio = Nothing,
        tool_calls = Nothing
      }

-- | Call the OpenAI API using servant openai bindings.
callLLM :: AgentConfig -> [Message] -> IO (Either Text Text)
callLLM config messages = do
  let methods = makeMethods config.clientEnv config.apiKey Nothing Nothing
      openAIMessages = Vector.fromList (map toOpenAIMessage messages)
      request =
        _CreateChatCompletion
          { Chat.messages = openAIMessages,
            Chat.model = config.model
          }
  result <- try @SomeException (methods.createChatCompletion request)
  pure $ case result of
    Left err -> Left (T.pack (show err))
    Right ChatCompletionObject {choices} ->
      case Vector.uncons choices of
        Nothing -> Left "No choices returned from API"
        Just (Choice {message}, _) -> Right (messageToContent message)

-- | Create initial messages with the system prompt from the toolkit.
initialMessages :: Toolkit db -> [Message]
initialMessages toolkit = [Message System toolkit.systemPrompt]

-- | Run the agent with a user query and conversation history.
-- Returns the response and the updated conversation history.
runAgent :: AgentConfig -> Toolkit db -> db -> [Message] -> Text -> IO (Text, [Message])
runAgent config toolkit db history userQuery = do
  putDispLn (AgentStart userQuery)

  let messages = history <> [Message User userQuery]

  (response, finalMessages) <- agentLoop config toolkit db messages 0
  pure (response, finalMessages)

-- | The main agent loop implementing ReAct pattern.
-- Returns the response and the final conversation history.
agentLoop :: AgentConfig -> Toolkit db -> db -> [Message] -> Int -> IO (Text, [Message])
agentLoop config toolkit db messages iteration
  | iteration >= config.maxIterations = do
      putDocLn "Max iterations reached"
      let response = "I apologize, but I was unable to complete your request. Please try again or contact support."
      pure (response, messages <> [Message Assistant response])
  | otherwise = do
      putDispLn (Iteration (iteration + 1))

      result <- callLLM config messages
      case result of
        Left err -> do
          putDispLn (Error err)
          let response = "I encountered an error: " <> err
          pure (response, messages <> [Message Assistant response])
        Right response -> do
          case parseResponse response of
            ParsedFinalAnswer thought answer -> do
              putDispLn (Thinking thought)
              putDispLn (FinalAnswer answer)
              pure (answer, messages <> [Message Assistant response])
            ActionRequest thought tool input -> do
              putDispLn (Thinking thought)
              putDispLn (ToolUse tool input)

              let toolResult = toolkit.executeTool tool input db
                  observation = case toolResult of
                    ToolSuccess txt -> txt
                    ToolError err -> "ERROR: " <> err

              putDispLn (Observation observation)

              let newMessages =
                    messages
                      <> [ Message Assistant response,
                           Message User ("Observation: " <> observation)
                         ]

              agentLoop config toolkit db newMessages (iteration + 1)
            InvalidFormat _ -> do
              -- LLM gave a conversational response without ReAct format - this is fine
              -- for simple questions or clarifications
              putDispLn (Response response)
              pure (response, messages <> [Message Assistant response])
