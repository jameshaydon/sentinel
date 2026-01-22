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
import Data.IORef (readIORef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Data.Vector qualified as V
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.Context (AskableSpec (..), ContextDecl (..), ContextStore)
import Sentinel.LLM (Message (..))
import Sentinel.LLM qualified as LLM
import Sentinel.Output qualified as Output
import Sentinel.Sentinel (PendingUserInput (..), Sentinel, SentinelEnv (..), SentinelResult (..))
import Sentinel.Sentinel qualified as Sentinel
import Sentinel.Verbosity (Verbosity (..))
import Sentinel.Solver.Askable (AskableDecl (..), formatQuestion)
import Sentinel.Solver.Types (Scalar (..), UserInputType (..), scalarToText)
import Sentinel.Tool (SideSessionSpec (..))
import Sentinel.Tool qualified as Tool
import Sentinel.Toolkit (Toolkit)
import Sentinel.Toolkit qualified as Toolkit

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
-- Blocked items are queried directly from Sentinel (no duplicate state).
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
    -- | Base tool definitions for LLM (schemas and descriptions).
    tools :: [Tool.LLMTool],
    -- | System prompt for the LLM.
    systemPrompt :: Text,
    -- | Sentinel for guarded tool calls.
    sentinel :: Sentinel db,
    -- | Sentinel environment (shared state for db and facts).
    sentinelEnv :: SentinelEnv db,
    -- | The toolkit (needed for dynamic Ask tool generation).
    toolkit :: Toolkit db
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

-- | Process a single tool call via Sentinel or handle dynamic Ask tools.
-- Returns a message for the LLM with the tool result or block information.
processToolCall :: ToolCall.ToolCall -> AgentM db Message
processToolCall (ToolCall.ToolCall_Function {id = callId, function = fn}) = do
  let toolName = fn.name
      argsJson = fn.arguments

  -- Check if this is a dynamic Ask tool (Ask_<name>)
  if "Ask_" `T.isPrefixOf` toolName
    then handleAskTool callId toolName
    else handleRegularTool callId toolName argsJson

-- | Handle a dynamic Ask tool call by triggering a side session.
handleAskTool :: Text -> Text -> AgentM db Message
handleAskTool callId toolName = do
  env <- ask
  let name = T.drop 4 toolName -- Remove "Ask_" prefix

  -- Query Sentinel directly to check if this is a pending context or askable
  mPendingCtx <- liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.findPendingContext name)
  mPendingAsk <- liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.findPendingAskable name)

  case mPendingCtx of
    Just _pending -> do
      -- Context variable side session
      case Toolkit.lookupContextDecl name env.toolkit.contextDecls of
        Nothing -> pure $ ToolMsg callId ("ERROR: Unknown context variable: " <> name)
        Just decl -> do
          let spec = ContextSession name decl
          summary <- runSideSession spec
          -- Clear from Sentinel's pending list
          liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.clearPendingUserInput name)
          pure $ ToolMsg callId summary
    Nothing -> case mPendingAsk of
      Just pending -> do
        -- Askable side session
        case Toolkit.lookupAskable name env.toolkit.askables of
          Nothing -> pure $ ToolMsg callId ("ERROR: Unknown askable predicate: " <> name)
          Just decl -> do
            let spec = AskableSession name decl pending.pendingArguments
            summary <- runSideSession spec
            -- Clear from Sentinel's pending list
            liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.clearPendingUserInput name)
            pure $ ToolMsg callId summary
      Nothing ->
        pure $ ToolMsg callId ("ERROR: " <> name <> " is not currently blocked. Call the relevant action tool first.")

-- | Handle a regular tool call through Sentinel.
handleRegularTool :: Text -> Text -> Text -> AgentM db Message
handleRegularTool callId toolName argsJson = do
  env <- ask

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
          -- Sentinel already registered the pending user input
          -- Return block information for the LLM
          let blockInfo = formatBlockForLLM question
          pure $ ToolMsg callId blockInfo

-- | Format blocked user question for the LLM to understand what to do.
formatBlockForLLM :: Sentinel.UserQuestion -> Text
formatBlockForLLM question =
  T.unlines
    [ "BLOCKED: Guard cannot proceed without additional information.",
      "",
      "Type: " <> Sentinel.describeUserInput question,
      "Question: " <> question.questionText,
      "",
      case question.inputType of
        AskableInput ->
          "To resolve: Call Ask_" <> question.inputName <> " to prompt the user."
            <> "\n  Arguments: "
            <> formatArgs question.arguments
        ContextInput ->
          "To resolve: Call Ask_" <> question.inputName <> " to prompt the user."
    ]
  where
    formatArgs args = "[" <> T.intercalate ", " (scalarToText <$> args) <> "]"

--------------------------------------------------------------------------------
-- Side Session
--------------------------------------------------------------------------------

-- | Run a synchronous side conversation session.
--
-- This prints the exact question to the user, waits for their response,
-- then calls a small LLM session with only Set/Confirm/Deny tools to
-- parse the response and establish the fact/context.
runSideSession :: SideSessionSpec -> AgentM db Text
runSideSession spec = do
  env <- ask
  let (question, sidePrompt) = case spec of
        ContextSession name decl ->
          let q = maybe ("Please specify " <> name) (.questionTemplate) decl.askable
              p = formatContextSidePrompt decl q
           in (q, p)
        AskableSession _predName decl args ->
          let q = formatQuestion decl.questionTemplate args
              p = formatAskableSidePrompt decl q
           in (q, p)

  -- 1. Print exact question to user
  liftIO $ putDispLn (Output.AskingUser question)

  -- 2. Get user response via blocking stdin read
  userResponse <- T.pack <$> liftIO getLine

  -- 3. Call LLM with the side session prompt
  let fullPrompt = sidePrompt userResponse
  result <- liftIO $ LLM.callChatCompletion env.config.llmConfig fullPrompt [] []

  -- 4. Process result
  processSideSessionResult spec question userResponse result

-- | Format the side session prompt for context variables.
formatContextSidePrompt :: ContextDecl -> Text -> Text -> Text
formatContextSidePrompt decl question userResponse =
  T.unlines
    [ "Extract the value from this user response.",
      "",
      "Question asked: " <> question,
      "User response: " <> userResponse,
      "",
      "If the user provided a clear value for '" <> decl.name <> "',",
      "respond with exactly: VALUE: <the value>",
      "",
      "If the response is unclear or doesn't provide the value,",
      "respond with exactly: AMBIGUOUS",
      "",
      "Examples:",
      "- User says 'The one to Paris, BK-123' -> VALUE: BK-123",
      "- User says 'booking REF456' -> VALUE: REF456",
      "- User says 'I'm not sure' -> AMBIGUOUS"
    ]

-- | Format the side session prompt for askable predicates.
formatAskableSidePrompt :: AskableDecl -> Text -> Text -> Text
formatAskableSidePrompt _decl question userResponse =
  T.unlines
    [ "Determine if the user confirmed or denied.",
      "",
      "Question asked: " <> question,
      "User response: " <> userResponse,
      "",
      "Based on the user's response:",
      "- If user CLEARLY agreed/confirmed (yes, correct, I confirm, etc), respond: CONFIRMED",
      "- If user CLEARLY disagreed/denied (no, incorrect, I don't, etc), respond: DENIED",
      "- If ambiguous or unclear, respond: AMBIGUOUS",
      "",
      "Respond with exactly one word: CONFIRMED, DENIED, or AMBIGUOUS"
    ]

-- | Process the result of a side session LLM call.
processSideSessionResult ::
  SideSessionSpec ->
  Text -> -- question
  Text -> -- userResponse
  Either Text (Chat.Message Text) ->
  AgentM db Text
processSideSessionResult spec question userResponse result = do
  env <- ask
  case result of
    Left err -> do
      liftIO $ putDispLn (Output.SideSessionAmbiguous question userResponse)
      pure $ formatAmbiguousSummary question userResponse ("LLM error: " <> err)
    Right msg -> case msg of
      Chat.Assistant {assistant_content = Just content} -> do
        let normalized = T.strip (T.toUpper content)
        case spec of
          ContextSession name _decl ->
            if "VALUE:" `T.isPrefixOf` normalized
              then do
                let extractedValue = T.strip $ T.drop 6 (T.strip content)
                    scalar = ScStr extractedValue
                -- Set the context value in Sentinel
                liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setContextValue name scalar [])
                let resultText = "Set " <> name <> " = " <> extractedValue
                liftIO $ putDispLn (Output.SideSessionSuccess question userResponse resultText)
                pure $ formatSuccessSummary question userResponse resultText
              else do
                liftIO $ putDispLn (Output.SideSessionAmbiguous question userResponse)
                pure $ formatAmbiguousSummary question userResponse "Could not extract value"
          AskableSession predName _decl args ->
            if "CONFIRMED" `T.isInfixOf` normalized
              then do
                liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setAskableFact predName args True)
                let resultText = "Confirmed: " <> predName
                liftIO $ putDispLn (Output.SideSessionSuccess question userResponse resultText)
                pure $ formatSuccessSummary question userResponse resultText
              else
                if "DENIED" `T.isInfixOf` normalized
                  then do
                    liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setAskableFact predName args False)
                    let resultText = "Denied: " <> predName
                    liftIO $ putDispLn (Output.SideSessionSuccess question userResponse resultText)
                    pure $ formatSuccessSummary question userResponse resultText
                  else do
                    liftIO $ putDispLn (Output.SideSessionAmbiguous question userResponse)
                    pure $ formatAmbiguousSummary question userResponse "Could not determine confirmation"
      _ -> do
        liftIO $ putDispLn (Output.SideSessionAmbiguous question userResponse)
        pure $ formatAmbiguousSummary question userResponse "Unexpected LLM response format"

-- | Format a successful side session summary for the LLM.
formatSuccessSummary :: Text -> Text -> Text -> Text
formatSuccessSummary question userResponse resultText =
  T.unlines
    [ "Side Session Complete:",
      "  Question: " <> question,
      "  User said: " <> userResponse,
      "  Result: " <> resultText
    ]

-- | Format an ambiguous side session summary for the LLM.
formatAmbiguousSummary :: Text -> Text -> Text -> Text
formatAmbiguousSummary question userResponse reason =
  T.unlines
    [ "Side Session - Ambiguous:",
      "  Question: " <> question,
      "  User said: " <> userResponse,
      "  Issue: " <> reason,
      "  The fact/context was NOT established. Consider asking a follow-up question."
    ]

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

-- | Run the agent with a user query.
--
-- Takes:
-- - Agent configuration
-- - Toolkit (for tool definitions and dynamic Ask tool generation)
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
  Toolkit db ->
  Text ->
  Sentinel db ->
  SentinelEnv db ->
  [Message] ->
  Int ->
  Text ->
  IO (Text, [Message], Int)
runAgent config toolkit baseSystemPrompt sentinel sentinelEnv history turnCount userQuery = do
  putDispLn (Output.TurnStart turnCount userQuery)
  let newTurnCount = turnCount + 1

  -- Get current context and augment system prompt with it
  ctxStore <- Sentinel.runSentinelM sentinelEnv Sentinel.getContextStore
  let systemPrompt = augmentSystemPromptWithContext baseSystemPrompt ctxStore

  let env =
        AgentEnv
          { config,
            tools = Toolkit.toLLMTools toolkit,
            systemPrompt,
            sentinel,
            sentinelEnv,
            toolkit
          }
      initialState =
        AgentState
          { messages = UserMsg userQuery : history,
            turnCount = newTurnCount,
            iterationCount = 0
          }
  (response, finalState) <- runAgentM env initialState agentLoop
  pure (response, finalState.messages, finalState.turnCount)

-- | Augment the system prompt with current context information.
augmentSystemPromptWithContext :: Text -> ContextStore -> Text
augmentSystemPromptWithContext basePrompt ctxStore =
  let contextText = Sentinel.formatContextForLLM ctxStore
   in if contextText == "No context established."
        then basePrompt
        else basePrompt <> "\n\n---\nSESSION CONTEXT:\n" <> contextText

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

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

      -- Query Sentinel directly for pending user inputs
      blockedCtx <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getPendingContextVars
      blockedAsk <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getPendingAskables

      -- Generate dynamic Ask tools based on pending inputs
      let dynamicTools = Toolkit.makeDynamicAskTools env.toolkit blockedCtx blockedAsk
          allTools = env.tools ++ (Tool.toLLMTool <$> dynamicTools)
          openAITools = Tool.toOpenAITool <$> allTools

      -- Log available tools before LLM call (if debug enabled)
      verbosityLevel <- liftIO $ readIORef env.sentinelEnv.verbosity
      when (verbosityLevel >= Basic) $ liftIO $ do
        putStrLn $ "[DEBUG] blockedContextVars: " <> show blockedCtx
        putStrLn $ "[DEBUG] blockedAskables: " <> show blockedAsk
        putStrLn $ "[DEBUG] Tools available to LLM: " <> show ((.name) <$> allTools)

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
      Just calls | not (V.null calls) -> do
        #messages %= (AssistantMsg assistant_content (Just calls) :)
        toolMsgs <- traverse processToolCall (V.toList calls)
        -- Add all tool results to messages and continue the loop.
        -- The LLM will see any BLOCKED messages and can call the appropriate
        -- Ask tools to prompt the user.
        #messages %= (reverse toolMsgs ++)
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
