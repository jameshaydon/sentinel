-- | Side session tool definitions and response processing.
--
-- Side sessions are synchronous mini-conversations for establishing context
-- variables or confirming askable predicates. This module provides:
-- - Tool definitions for side session LLM calls
-- - Response processing to extract results from tool calls or text
module Sentinel.SideSession
  ( -- * Side Session Results
    SideSessionResult (..),

    -- * Tool Definitions
    contextSessionTools,
    askableSessionTools,

    -- * Response Processing
    processSideSessionResponse,

    -- * Prompt Formatting
    formatContextSidePrompt,
    formatAskableSidePrompt,
  )
where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Data.Vector qualified as V
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.Tool qualified as OpenAI
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.JSON (extractString, extractToolArg)
import Sentinel.Schema (boolProp, emptyObjectSchema, numberProp, objectSchema, stringProp)
import Sentinel.Solver.Askable (AskableDecl)
import Sentinel.Solver.Types (Scalar (..), ScalarType (..))

--------------------------------------------------------------------------------
-- Side Session Results
--------------------------------------------------------------------------------

-- | Result of a side session.
data SideSessionResult
  = -- | User provided a value for a context variable
    ValueSet Scalar
  | -- | User confirmed an askable predicate
    Confirmed
  | -- | User denied an askable predicate
    Denied
  | -- | Response was ambiguous or unclear
    Ambiguous Text
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Tool Definitions for Context Sessions
--------------------------------------------------------------------------------

-- | Generate tools for a context session based on the expected value type.
--
-- Different tools are provided based on the expected type:
-- - TextType/IntType/FloatType: UserAnsweredWithString/Number tool
-- - BoolType: UserAnsweredWithBoolean tool
--
-- All sessions also include an Ambiguous tool for unclear responses.
contextSessionTools :: ScalarType -> [OpenAI.Tool]
contextSessionTools = \case
  TextType -> [userAnsweredStringTool, ambiguousTool]
  IntType -> [userAnsweredNumberTool, ambiguousTool]
  FloatType -> [userAnsweredNumberTool, ambiguousTool]
  BoolType -> [userAnsweredBoolTool, ambiguousTool]
  ExprType -> [userAnsweredStringTool, ambiguousTool]

-- | Tool for when user provides a string value.
userAnsweredStringTool :: OpenAI.Tool
userAnsweredStringTool =
  mkTool
    "UserAnsweredWithString"
    "Call this when the user provided a clear string value in their response."
    $ objectSchema
      [("value", stringProp "The string value provided by the user")]
      ["value"]

-- | Tool for when user provides a numeric value.
userAnsweredNumberTool :: OpenAI.Tool
userAnsweredNumberTool =
  mkTool
    "UserAnsweredWithNumber"
    "Call this when the user provided a clear numeric value in their response."
    $ objectSchema
      [("value", numberProp "The numeric value provided by the user")]
      ["value"]

-- | Tool for when user provides a boolean value.
userAnsweredBoolTool :: OpenAI.Tool
userAnsweredBoolTool =
  mkTool
    "UserAnsweredWithBoolean"
    "Call this when the user provided a clear yes/no or true/false answer."
    $ objectSchema
      [("value", boolProp "True if user said yes/true, false if no/false")]
      ["value"]

--------------------------------------------------------------------------------
-- Tool Definitions for Askable Sessions
--------------------------------------------------------------------------------

-- | Tools for askable (confirm/deny) sessions.
askableSessionTools :: [OpenAI.Tool]
askableSessionTools = [confirmTool, denyTool, ambiguousTool]

-- | Tool for when user confirms.
confirmTool :: OpenAI.Tool
confirmTool =
  mkTool
    "Confirm"
    "Call this when the user clearly agreed, confirmed, or said yes."
    emptyObjectSchema

-- | Tool for when user denies.
denyTool :: OpenAI.Tool
denyTool =
  mkTool
    "Deny"
    "Call this when the user clearly disagreed, denied, or said no."
    emptyObjectSchema

-- | Tool for ambiguous responses.
ambiguousTool :: OpenAI.Tool
ambiguousTool =
  mkTool
    "Ambiguous"
    "Call this when the user's response is unclear, doesn't answer the question, or you cannot determine their intent."
    $ objectSchema
      [("reason", stringProp "Brief explanation of why the response is ambiguous")]
      ["reason"]

-- | Helper to construct an OpenAI tool with strict mode.
mkTool :: Text -> Text -> Aeson.Value -> OpenAI.Tool
mkTool name desc params =
  OpenAI.Tool_Function
    { function =
        OpenAI.Function
          { name = name,
            description = Just desc,
            parameters = Just params,
            strict = Just True
          }
    }

--------------------------------------------------------------------------------
-- Response Processing
--------------------------------------------------------------------------------

-- | Process an LLM response from a side session.
--
-- Priority:
-- 1. If the LLM called a tool, process the tool call
-- 2. Otherwise, fall back to text parsing for backwards compatibility
processSideSessionResponse :: Either Text (Chat.Message Text) -> SideSessionResult
processSideSessionResponse = \case
  Left err ->
    Ambiguous ("LLM error: " <> err)
  Right msg -> case msg of
    Chat.Assistant {tool_calls = Just calls}
      | not (V.null calls) ->
          -- Process the first tool call
          processToolCall (V.head calls)
    Chat.Assistant {assistant_content = Just content} ->
      -- Fall back to text parsing
      parseTextResponse content
    _ ->
      Ambiguous "Unexpected LLM response format"

-- | Process a tool call from the side session LLM.
processToolCall :: ToolCall.ToolCall -> SideSessionResult
processToolCall (ToolCall.ToolCall_Function {function = fn}) =
  case fn.name of
    "UserAnsweredWithString" ->
      case extractToolArg "value" =<< decodeArgs fn.arguments of
        Just scalar -> ValueSet scalar
        _ -> Ambiguous "Could not parse string value from tool call"
    "UserAnsweredWithNumber" ->
      case extractToolArg "value" =<< decodeArgs fn.arguments of
        Just scalar -> ValueSet scalar
        _ -> Ambiguous "Could not parse number value from tool call"
    "UserAnsweredWithBoolean" ->
      case extractToolArg "value" =<< decodeArgs fn.arguments of
        Just scalar -> ValueSet scalar
        _ -> Ambiguous "Could not parse boolean value from tool call"
    "Confirm" ->
      Confirmed
    "Deny" ->
      Denied
    "Ambiguous" ->
      Ambiguous $ fromMaybe "No reason provided" $ do
        args <- decodeArgs fn.arguments
        extractString "reason" args
    other ->
      Ambiguous ("Unknown tool called: " <> other)

-- | Decode tool call arguments from JSON text.
decodeArgs :: Text -> Maybe Aeson.Value
decodeArgs = Aeson.decodeStrict . T.Encoding.encodeUtf8

-- | Fall back to text parsing for backwards compatibility.
--
-- This handles cases where the LLM might respond with plain text
-- instead of using the tools (should be rare with proper prompting).
parseTextResponse :: Text -> SideSessionResult
parseTextResponse content =
  let normalized = T.strip (T.toUpper content)
   in if
        | "VALUE:" `T.isPrefixOf` normalized ->
            let extractedValue = T.strip $ T.drop 6 (T.strip content)
             in ValueSet (ScStr extractedValue)
        | "CONFIRMED" `T.isInfixOf` normalized ->
            Confirmed
        | "DENIED" `T.isInfixOf` normalized ->
            Denied
        | otherwise ->
            Ambiguous "Could not parse text response"

--------------------------------------------------------------------------------
-- Prompt Formatting
--------------------------------------------------------------------------------

-- | Format the side session prompt for context variables.
--
-- The prompt instructs the LLM to use the appropriate tool based on value type.
formatContextSidePrompt :: ScalarType -> Text -> Text -> Text
formatContextSidePrompt valueType question userResponse =
  T.unlines
    [ "Extract the " <> typeName <> " value from this user response.",
      "",
      "Question asked: " <> question,
      "User response: " <> userResponse,
      "",
      "Instructions:",
      "- If the user provided a clear " <> typeName <> " value, call the " <> toolName <> " tool with the value.",
      "- If the response is unclear or doesn't provide the value, call the Ambiguous tool.",
      "",
      "You MUST call exactly one of the available tools."
    ]
  where
    (typeName, toolName) = case valueType of
      TextType -> ("string", "UserAnsweredWithString")
      IntType -> ("numeric", "UserAnsweredWithNumber")
      FloatType -> ("numeric", "UserAnsweredWithNumber")
      BoolType -> ("boolean (yes/no)", "UserAnsweredWithBoolean")
      ExprType -> ("expression", "UserAnsweredWithString")

-- | Format the side session prompt for askable predicates.
formatAskableSidePrompt :: AskableDecl -> Text -> Text -> Text
formatAskableSidePrompt _decl question userResponse =
  T.unlines
    [ "Determine if the user confirmed or denied.",
      "",
      "Question asked: " <> question,
      "User response: " <> userResponse,
      "",
      "Instructions:",
      "- If the user clearly agreed, confirmed, or said yes, call the Confirm tool.",
      "- If the user clearly disagreed, denied, or said no, call the Deny tool.",
      "- If the response is unclear or ambiguous, call the Ambiguous tool.",
      "",
      "You MUST call exactly one of these tools."
    ]
