module Sentinel.LLM
  ( LLMConfig (..),
    Model,
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
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.Verbosity (Verbosity (..))
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
        model = "gpt-4.1-nano-2025-04-14"
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

-- | Log a tool call for debugging (compact single-line format).
logToolCall :: (Text -> IO ()) -> ToolCall -> IO ()
logToolCall emitDebug (ToolCall.ToolCall_Function {function = fn}) =
  emitDebug $ "[LLM Tool Call] " <> fn.name <> " " <> fn.arguments

-- | Log all tool calls from an LLM response (if verbosity >= Basic).
logToolCalls :: (Text -> IO ()) -> Verbosity -> Chat.Message Text -> IO ()
logToolCalls emitDebug verbosity = \case
  Chat.Assistant {tool_calls = Just calls}
    | verbosity >= Basic && not (Vector.null calls) ->
        traverse_ (logToolCall emitDebug) calls
  _ -> pure ()

-- | Log request details at Verbose level.
logRequest :: (Text -> IO ()) -> Verbosity -> Vector (Chat.Message (Vector Chat.Content)) -> [OpenAI.Tool] -> IO ()
logRequest emitDebug verbosity msgs tools = when (verbosity >= Verbose) do
  emitDebug $ "[LLM Request] " <> T.pack (show (Vector.length msgs)) <> " messages"
  forM_ (Vector.toList msgs) \msg -> emitDebug $ T.pack (formatMessage msg)
  unless (null tools) do
    emitDebug $ "[LLM Tools] " <> T.pack (show (length tools)) <> " tools"
    forM_ tools \case
      OpenAI.Tool_Function fn -> emitDebug $ "- " <> fn.name
      OpenAI.Tool_Code_Interpreter _ -> emitDebug "- (code_interpreter)"
      OpenAI.Tool_File_Search _ -> emitDebug "- (file_search)"
      OpenAI.Tool_Web_Search -> emitDebug "- (web_search)"

-- | Format a message for logging.
formatMessage :: Chat.Message (Vector Chat.Content) -> String
formatMessage = \case
  Chat.System {content} -> "[System] " <> extractText content
  Chat.User {content} -> "[User] " <> extractText content
  Chat.Assistant {assistant_content, tool_calls} ->
    "[Assistant] "
      <> maybe "(no content)" extractText assistant_content
      <> maybe "" (\tc -> " + " <> show (Vector.length tc) <> " tool calls") tool_calls
  Chat.Tool {tool_call_id, content} ->
    "[Tool:" <> T.unpack tool_call_id <> "] " <> extractText content

-- | Extract text from content vector.
extractText :: Vector Chat.Content -> String
extractText contents =
  T.unpack
    . T.intercalate " "
    $ Vector.toList contents
    >>= \case
      Chat.Text {text} -> [text]
      _ -> []

-- | Log response details at Verbose level.
logResponse :: (Text -> IO ()) -> Verbosity -> Chat.Message Text -> IO ()
logResponse emitDebug verbosity msg = when (verbosity >= Verbose) do
  case msg of
    Chat.Assistant {assistant_content, tool_calls} -> do
      emitDebug $ "[LLM Response] " <> fromMaybe "(no content)" assistant_content
      case tool_calls of
        Nothing -> pure ()
        Just calls -> do
          emitDebug $ "[LLM Response Tool Calls] " <> T.pack (show (Vector.length calls))
          forM_ (Vector.toList calls) \(ToolCall.ToolCall_Function {id = tcId, function = fn}) ->
            emitDebug $ "- " <> tcId <> ": " <> fn.name <> "(" <> fn.arguments <> ")"
    _ -> emitDebug $ "[LLM Response] (unexpected message type: " <> T.pack (show msg) <> ")"

-- | Call the LLM chat completion API.
-- The optional finalInstructions parameter appends a system message at the end,
-- making it the last thing the LLM sees before generating a response.
callChatCompletion ::
  Verbosity ->
  (Text -> IO ()) ->
  LLMConfig ->
  Text ->
  [Message] ->
  [OpenAI.Tool] ->
  Maybe Text ->
  IO (Either Text (Chat.Message Text))
callChatCompletion verbosity emitDebug config systemPrompt msgs tools finalInstructions = do
  let systemMsg =
        Chat.System
          { content = Vector.singleton (Chat.Text {text = systemPrompt}),
            name = Nothing
          }
      conversationMsgs = Vector.fromList (map toOpenAIMessage msgs)
      finalMsg = case finalInstructions of
        Nothing -> Vector.empty
        Just txt ->
          Vector.singleton
            $ Chat.System
              { content = Vector.singleton (Chat.Text {text = txt}),
                name = Nothing
              }
      openAIMessages =
        Vector.cons systemMsg (conversationMsgs <> finalMsg)
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
  logRequest emitDebug verbosity openAIMessages tools
  result <- try @SomeException (methods.createChatCompletion request)
  case result of
    Left err -> do
      when (verbosity >= Verbose)
        $ emitDebug
        $ "[LLM Error] "
        <> T.pack (show err)
      pure $ Left (T.pack (show err))
    Right ChatCompletionObject {choices} ->
      case Vector.uncons choices of
        Nothing -> pure $ Left "No choices returned from API"
        Just (Choice {message}, _) -> do
          logResponse emitDebug verbosity message
          logToolCalls emitDebug verbosity message
          pure $ Right message
