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
import Sentinel.Sentinel (EventSink (..), InputKind (..), InputMeta (..), PendingUserInput (..), Sentinel, SentinelEnv (..), SentinelResult (..))
import Sentinel.Sentinel qualified as Sentinel
import Sentinel.SideSession qualified as SideSession
import Sentinel.Solver.Askable (AskableDecl (..), formatQuestion)
import Sentinel.Solver.Types (Scalar (..), scalarToText)
import Sentinel.Tool (SideSessionSpec (..))
import Sentinel.Tool qualified as Tool
import Sentinel.Toolkit (Toolkit)
import Sentinel.Toolkit qualified as Toolkit
import Sentinel.Verbosity (Verbosity (..))

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
-- Agent Event Emission
--------------------------------------------------------------------------------

-- | Emit a display event through the sentinel environment's 'EventSink'.
emitAgentEvent :: Doc Ann -> AgentM db ()
emitAgentEvent doc = do
  env <- ask
  liftIO $ env.sentinelEnv.eventSink.emit doc

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

callLLM :: Maybe Text -> [LLM.Tool] -> AgentM db (Either Text (Chat.Message Text))
callLLM finalInstructions llmTools = do
  env <- ask
  msgs <- use #messages
  verbosity <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getVerbosity
  let trimmedMsgs = trimHistory env.config.maxMessages msgs
      -- Reverse to get chronological order for the API
      chronologicalMsgs = reverse trimmedMsgs
      emitDebug t = env.sentinelEnv.eventSink.emit (pretty t)
  liftIO $ LLM.callChatCompletion verbosity emitDebug env.config.llmConfig env.systemPrompt chronologicalMsgs llmTools finalInstructions

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
    Just pending -> do
      -- Context variable side session
      case Toolkit.lookupContextDecl name env.toolkit.contextDecls of
        Nothing -> pure $ ToolMsg callId ("ERROR: Unknown context variable: " <> name)
        Just decl -> do
          let spec = ContextSession name decl pending.pendingCandidates
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
      emitAgentEvent (disp (Output.ToolError err))
      pure $ ToolMsg callId ("ERROR: " <> err)
    Just args -> do
      -- Call Sentinel to guard and execute the tool
      result <- liftIO $ Sentinel.runSentinelM env.sentinelEnv (env.sentinel.guardedCall toolName args)
      case result of
        Allowed obs -> do
          pure $ ToolMsg callId obs
        Denied reason -> do
          pure $ ToolMsg callId ("ERROR: " <> reason)
        AskUser _question blockText -> do
          -- Sentinel already registered the pending user input
          -- Return the formatted solver outcome text for the LLM
          pure $ ToolMsg callId blockText

--------------------------------------------------------------------------------
-- Side Session
--------------------------------------------------------------------------------

-- | Run a synchronous side conversation session.
--
-- This prints the exact question to the user, waits for their response,
-- then calls a small LLM session with typed tools (UserAnsweredWith*, Confirm,
-- Deny, Ambiguous) to parse the response and establish the fact/context.
runSideSession :: SideSessionSpec -> AgentM db Text
runSideSession spec = do
  env <- ask
  let (question, sidePrompt, sideTools) = case spec of
        ContextSession name decl _candidates ->
          let q = maybe ("Please specify " <> name) (.questionTemplate) decl.askable
              p = SideSession.formatContextSidePrompt decl.valueType q
              tools = SideSession.contextSessionTools decl.valueType
           in (q, p, tools)
        AskableSession _predName decl args ->
          let q = formatQuestion decl.questionTemplate args
              p = SideSession.formatAskableSidePrompt decl q
              tools = SideSession.askableSessionTools
           in (q, p, tools)

  -- Build structured InputMeta for the frontend
  let meta = case spec of
        ContextSession name _decl pendingCandidates ->
          InputMeta
            { question = question,
              inputKind = ContextInputKind,
              inputName = name,
              candidates = map scalarToText pendingCandidates
            }
        AskableSession predName _decl _args ->
          InputMeta
            { question = question,
              inputKind = AskableInputKind,
              inputName = predName,
              candidates = []
            }

  -- 1. Print exact question to user
  emitAgentEvent (disp (Output.AskingUser question))

  -- 2. Get user response via the UserInput abstraction
  userResponse <- liftIO $ env.sentinelEnv.userInput.getSideInput meta

  -- 3. Call LLM with the side session prompt and tools
  verbosity <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getVerbosity
  let fullPrompt = sidePrompt userResponse
      emitDebug t = env.sentinelEnv.eventSink.emit (pretty t)
  result <- liftIO $ LLM.callChatCompletion verbosity emitDebug env.config.llmConfig fullPrompt [] sideTools Nothing

  -- 4. Process result using SideSession module
  let sideResult = SideSession.processSideSessionResponse result
  applySideSessionResult spec question userResponse sideResult

-- | Apply the result of a side session to establish facts/context.
applySideSessionResult ::
  SideSessionSpec ->
  Text -> -- question
  Text -> -- userResponse
  SideSession.SideSessionResult ->
  AgentM db Text
applySideSessionResult spec question userResponse = \case
  SideSession.ValueSet scalar -> case spec of
    ContextSession name _decl _candidates -> do
      env <- ask
      -- Set the context value in Sentinel
      liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setContextValue name scalar [])
      let resultText = "Set " <> name <> " = " <> dispText scalar
      emitAgentEvent (disp (Output.SideSessionSuccess question userResponse resultText))
      pure $ formatSuccessSummary question userResponse resultText
    AskableSession {} -> do
      -- ValueSet doesn't make sense for askables, treat as ambiguous
      emitAgentEvent (disp (Output.SideSessionAmbiguous question userResponse))
      pure $ formatAmbiguousSummary question userResponse "Got value for askable (expected confirm/deny)"
  SideSession.Confirmed -> case spec of
    AskableSession predName _decl args -> do
      env <- ask
      liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setAskableFact predName args True)
      let resultText = "Confirmed: " <> predName
      emitAgentEvent (disp (Output.SideSessionSuccess question userResponse resultText))
      pure $ formatSuccessSummary question userResponse resultText
    ContextSession {} -> do
      -- Confirmed doesn't make sense for context, treat as ambiguous
      emitAgentEvent (disp (Output.SideSessionAmbiguous question userResponse))
      pure $ formatAmbiguousSummary question userResponse "Got confirmation for context (expected value)"
  SideSession.Denied -> case spec of
    AskableSession predName _decl args -> do
      env <- ask
      liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setAskableFact predName args False)
      let resultText = "Denied: " <> predName
      emitAgentEvent (disp (Output.SideSessionSuccess question userResponse resultText))
      pure $ formatSuccessSummary question userResponse resultText
    ContextSession {} -> do
      -- Denied doesn't make sense for context, treat as ambiguous
      emitAgentEvent (disp (Output.SideSessionAmbiguous question userResponse))
      pure $ formatAmbiguousSummary question userResponse "Got denial for context (expected value)"
  SideSession.Ambiguous reason -> do
    emitAgentEvent (disp (Output.SideSessionAmbiguous question userResponse))
    pure $ formatAmbiguousSummary question userResponse reason
-- | Format a successful side session summary for the LLM.
formatSuccessSummary :: Text -> Text -> Text -> Text
formatSuccessSummary question userResponse resultText =
  T.unlines
    [ "Side Session Complete:",
      "  Question: " <> question,
      "  User said: " <> userResponse,
      "  Result: " <> resultText,
      "",
      "New information has been established. You MUST re-run the relevant tool (e.g. CheckEligibility) to get updated results before giving a final answer."
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
  sentinelEnv.eventSink.emit (disp (Output.TurnStart turnCount userQuery))
  let newTurnCount = turnCount + 1

  -- Note: Context is now included in final instructions at the end of messages,
  -- rather than being appended to the system prompt at the start.
  let systemPrompt = baseSystemPrompt

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

-- | Generate final instructions including current context and pending questions.
-- These are appended at the end of the messages, making them the last thing
-- the LLM sees before generating a response.
--
-- When proofs have been found alongside blocked questions, the instructions
-- tell the LLM to present proofs first and let the user decide whether to
-- continue exploring, rather than unconditionally asking all blocked questions.
makeFinalInstructions :: Bool -> ContextStore -> [Text] -> [(Text, [Scalar])] -> Maybe Text
makeFinalInstructions proofsFound ctxStore blockedCtx blockedAskables =
  let contextText = Sentinel.formatContextForLLM ctxStore
      hasContext = contextText /= "No context established."
      hasPending = not (null blockedCtx && null blockedAskables)
   in if not hasContext && not hasPending
        then Nothing
        else Just $ T.unlines $ catMaybes [contextSection, pendingSection]
  where
    contextSection =
      let contextText = Sentinel.formatContextForLLM ctxStore
       in if contextText == "No context established."
            then Nothing
            else Just $ "CURRENT SESSION STATE:\n" <> contextText

    pendingSection =
      if null blockedCtx && null blockedAskables
        then Nothing
        else Just $ if proofsFound then proofsFoundPending else noProofsPending

    -- When proofs exist: present them, offer to continue
    proofsFoundPending =
      T.unlines
        [ "PROOFS HAVE BEEN FOUND. Present them to the user BEFORE doing anything else.",
          "There are also additional paths that could be explored if the user wants.",
          "Ask the user whether they would like to explore alternative eligibility paths.",
          "Do NOT call Ask_ tools unless the user explicitly asks to continue exploring.",
          "",
          "Available for further exploration (if the user requests it):",
          T.unlines
            $ ["  - Ask_" <> ctx | ctx <- blockedCtx]
            ++ ["  - Ask_" <> predName | (predName, _) <- blockedAskables]
        ]

    -- When no proofs yet: actively gather information
    noProofsPending =
      T.unlines
        [ "IMPORTANT: The following items are blocking progress:",
          "",
          if null blockedCtx
            then ""
            else
              "Blocked context variables:\n"
                <> T.unlines ["  - " <> ctx | ctx <- blockedCtx],
          if null blockedAskables
            then ""
            else
              "Blocked askables:\n"
                <> T.unlines ["  - " <> predName <> formatArgs args | (predName, args) <- blockedAskables],
          "Unless you are sure these questions are irrelevant to the user's request, "
            <> "call the relevant Ask_ tool to gather the needed information:",
          T.unlines
            $ ["  - Ask_" <> ctx | ctx <- blockedCtx]
            ++ ["  - Ask_" <> predName | (predName, _) <- blockedAskables],
          "Note that this tool must be invoked for Sentinel to register this. Even if it seems obvious from the previous conversation messages, it must be confirmed with the user through an Ask_ tool call."
        ]

    formatArgs [] = ""
    formatArgs args = "(" <> T.intercalate ", " (dispText <$> args) <> ")"

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

agentLoop :: AgentM db Text
agentLoop = do
  env <- ask
  iteration <- use #iterationCount
  if iteration > env.config.maxIterations
    then do
      emitAgentEvent (disp (Output.IterationLimitReached env.config.maxIterations))
      let response = "I apologize, but I was unable to complete your request. Please try again or contact support."
      #messages %= (AssistantMsg (Just response) Nothing :)
      pure response
    else do
      emitAgentEvent (disp (Output.Iteration iteration))
      #iterationCount += 1

      -- Query Sentinel directly for pending user inputs and current context
      blockedCtx <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getPendingContextVars
      blockedAsk <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getPendingAskables
      ctxStore <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getContextStore
      proofsFound <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getProofsFound

      -- Generate dynamic Ask tools based on pending inputs
      let dynamicTools = Toolkit.makeDynamicAskTools env.toolkit blockedCtx blockedAsk
          allTools = env.tools ++ (Tool.toLLMTool <$> dynamicTools)
          openAITools = Tool.toOpenAITool <$> allTools

      -- Generate final instructions with current context and pending questions
      -- These are appended at the end of messages, making them the last thing the LLM sees
      let finalInstructions = makeFinalInstructions proofsFound ctxStore blockedCtx blockedAsk

      -- Log available tools before LLM call (if debug enabled)
      verbosityLevel <- liftIO $ Sentinel.runSentinelM env.sentinelEnv Sentinel.getVerbosity
      when (verbosityLevel >= Basic) $ do
        emitAgentEvent (pretty $ "[DEBUG] blockedContextVars: " <> T.pack (show blockedCtx))
        emitAgentEvent (pretty $ "[DEBUG] blockedAskables: " <> T.pack (show blockedAsk))
        emitAgentEvent (pretty $ "[DEBUG] Tools available to LLM: " <> T.pack (show ((.name) <$> allTools)))

      result <- callLLM finalInstructions openAITools
      case result of
        Left err -> do
          emitAgentEvent (disp (Output.Error err))
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
        emitAgentEvent (disp (Output.FinalAnswer response))
        #messages %= (AssistantMsg (Just response) Nothing :)
        -- Reset proofsFound: the LLM has presented results to the user.
        -- If the user asks to continue exploring, the next turn should use
        -- the directive "go ask these questions" instructions.
        env <- ask
        liftIO $ Sentinel.runSentinelM env.sentinelEnv (Sentinel.setProofsFound False)
        pure response
  _ -> do
    let response = "Unexpected response format from LLM"
    #messages %= (AssistantMsg (Just response) Nothing :)
    pure response
