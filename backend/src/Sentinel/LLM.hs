module Sentinel.LLM
  ( LLMConfig (..),
    Message (..),
    Tool,
    defaultConfig,
    callChatCompletion,
    toOpenAIMessage,
  )
where

import Control.Exception (SomeException, try)
import Data.Text qualified as T
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
import OpenAI.V1.ToolCall (ToolCall)
import Pre
import Servant.Client (ClientEnv)

-- | Re-export of OpenAI Tool type.
type Tool = OpenAI.Tool

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for LLM API calls.
data LLMConfig = LLMConfig
  { clientEnv :: ClientEnv,
    apiKey :: Text,
    model :: Model
  }

-- | Create default configuration using OpenAI API.
defaultConfig :: Text -> IO LLMConfig
defaultConfig key = do
  clientEnv <- getClientEnv "https://api.openai.com"
  pure
    LLMConfig
      { clientEnv = clientEnv,
        apiKey = key,
        model = "gpt-5-nano-2025-08-07"
      }

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

-- | A message in the conversation (our internal representation).
data Message
  = UserMsg Text
  | AssistantMsg (Maybe Text) (Maybe (Vector ToolCall))
  | ToolMsg Text Text
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Message Conversion
--------------------------------------------------------------------------------

toOpenAIMessage :: Message -> Chat.Message (Vector Chat.Content)
toOpenAIMessage = \case
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
-- LLM API
--------------------------------------------------------------------------------

-- | Call the LLM chat completion API.
callChatCompletion ::
  LLMConfig ->
  Text ->
  [Message] ->
  [OpenAI.Tool] ->
  IO (Either Text (Chat.Message Text))
callChatCompletion config systemPrompt msgs tools = do
  let systemMsg =
        Chat.System
          { content = Vector.singleton (Chat.Text {text = systemPrompt}),
            name = Nothing
          }
      openAIMessages =
        Vector.cons systemMsg
          $ Vector.fromList (map toOpenAIMessage msgs)
      methods = makeMethods config.clientEnv config.apiKey Nothing Nothing
      request =
        _CreateChatCompletion
          { Chat.messages = openAIMessages,
            Chat.model = config.model,
            Chat.tools =
              if null tools
                then Nothing
                else Just (Vector.fromList tools),
            Chat.tool_choice =
              if null tools
                then Nothing
                else Just OpenAI.ToolChoiceAuto
          }
  result <- try @SomeException (methods.createChatCompletion request)
  pure $ case result of
    Left err -> Left (T.pack (show err))
    Right ChatCompletionObject {choices} ->
      case Vector.uncons choices of
        Nothing -> Left "No choices returned from API"
        Just (Choice {message}, _) -> Right message
