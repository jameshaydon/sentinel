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
import Data.Aeson.Text qualified as Aeson.Text
import Data.Text.Encoding qualified as T.Encoding
import Data.Text.Lazy qualified as T.Lazy
import Data.Vector qualified as Vector
import OpenAI.V1.Chat.Completions qualified as Chat
import OpenAI.V1.ToolCall qualified as ToolCall
import Pre
import Sentinel.Facts qualified as Facts
import Sentinel.Guard qualified as Guard
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
    maxMessages :: Int,
    -- | Maximum attempts to resolve a blocked guard by running queries.
    maxGuardResolutionAttempts :: Int
  }

-- | Create default configuration using OpenAI API.
defaultConfig :: Text -> IO AgentConfig
defaultConfig key = do
  llmConfig <- LLM.defaultConfig key
  pure
    AgentConfig
      { llmConfig = llmConfig,
        maxIterations = 10,
        maxMessages = 50,
        maxGuardResolutionAttempts = 3
      }

--------------------------------------------------------------------------------
-- AgentM Monad
--------------------------------------------------------------------------------

data AgentState db fact = AgentState
  { db :: db,
    -- | Accumulated facts during conversation.
    facts :: Facts.FactsDB fact,
    -- | Messages stored in reverse order (newest first) for efficient prepending.
    messages :: [Message],
    turnCount :: !Int,
    iterationCount :: !Int
  }
  deriving stock (Generic)

type AgentM db fact = ReaderT (AgentEnv db fact) (StateT (AgentState db fact) IO)

data AgentEnv db fact = AgentEnv
  { config :: AgentConfig,
    toolkit :: Tool.Toolkit (AgentM db fact) fact,
    systemPrompt :: Text
  }

runAgentM :: AgentEnv db fact -> AgentState db fact -> AgentM db fact a -> IO (a, AgentState db fact)
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

callLLM :: [LLM.Tool] -> AgentM db fact (Either Text (Chat.Message Text))
callLLM tools = do
  env <- ask
  msgs <- use #messages
  let trimmedMsgs = trimHistory env.config.maxMessages msgs
      -- Reverse to get chronological order for the API
      chronologicalMsgs = reverse trimmedMsgs
  liftIO $ LLM.callChatCompletion env.config.llmConfig env.systemPrompt chronologicalMsgs tools

--------------------------------------------------------------------------------
-- Guard Resolution
--------------------------------------------------------------------------------

-- | Execute a pending query to establish facts.
-- Returns True if the query succeeded and facts were added.
executeQuery :: (Ord fact) => Guard.PendingQuery -> AgentM db fact Bool
executeQuery query = do
  env <- ask
  let queryName = query.queryToolName
      queryArgs = query.queryArgs
      argsText = T.Lazy.toStrict (Aeson.Text.encodeToLazyText queryArgs)

  -- Display the query being executed
  liftIO $ putDispLn (Output.QueryExecution queryName argsText)

  -- Find the query tool
  let foundTool = find (\t -> t.toolName == queryName) env.toolkit.tools
  case foundTool of
    Nothing -> do
      liftIO $ putDispLn (Output.ToolError $ "Resolution query tool not found: " <> queryName)
      pure False
    Just tool -> do
      -- Execute the query (ignore its guard since query tools have trivial guards)
      result <- runExceptT (tool.toolAction queryArgs)
      case result of
        Left err -> do
          liftIO $ putDispLn (Output.ToolError err)
          pure False
        Right _obs -> do
          -- Extract and add facts from the query result
          producedFacts <- tool.toolFactsProduced queryArgs
          #facts %= Facts.addFacts producedFacts
          pure True

-- | Try to resolve a blocked guard by executing pending queries.
-- Returns the final guard result after resolution attempts.
resolveGuard ::
  (Ord fact) =>
  Int ->
  Tool.Tool (AgentM db fact) fact ->
  Aeson.Value ->
  AgentM db fact Guard.GuardResult
resolveGuard attemptsRemaining tool args = do
  currentFacts <- use #facts
  result <- liftIO $ Guard.evaluateGuard currentFacts (tool.toolGuard args)
  case result of
    Guard.GuardAllowed -> pure Guard.GuardAllowed
    Guard.GuardDenied failure -> pure (Guard.GuardDenied failure)
    Guard.NeedsResolution (Guard.RunQueries queries) | attemptsRemaining > 0 -> do
      -- Display resolution attempt
      let attemptNum = 4 - attemptsRemaining -- Assuming maxGuardResolutionAttempts = 3
      liftIO $ putDispLn (Output.ResolutionAttempt tool.toolName attemptNum (length queries))

      -- Execute each pending query
      results <- traverse executeQuery (toList queries)

      -- If at least one query succeeded, retry the guard
      if or results
        then resolveGuard (attemptsRemaining - 1) tool args
        else -- All queries failed, return the resolution need
          pure result
    other -> pure other

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Process a single tool call and return the result message.
processToolCall :: (Ord fact) => ToolCall.ToolCall -> AgentM db fact Message
processToolCall (ToolCall.ToolCall_Function {id = callId, function = fn}) = do
  env <- ask
  let toolName = fn.name
      argsJson = fn.arguments

  liftIO $ putDispLn (Output.ToolUse toolName argsJson)

  -- Decode the raw JSON string from OpenAI into a Value
  let mArgs = Aeson.decodeStrict (T.Encoding.encodeUtf8 argsJson) :: Maybe Aeson.Value

  result <- case mArgs of
    Nothing -> pure $ Left $ "Could not parse arguments JSON: " <> argsJson
    Just args -> do
      -- Find the tool in the list
      let foundTool = find (\t -> t.toolName == toolName) env.toolkit.tools
      case foundTool of
        Nothing -> pure $ Left $ "Tool not found: " <> toolName
        Just tool -> do
          -- Run the LogicT-based guard with auto-resolution
          maxAttempts <- asks (.config.maxGuardResolutionAttempts)
          guardResult <- resolveGuard maxAttempts tool args
          case guardResult of
            Guard.GuardDenied failure -> do
              let reason = formatGuardFailure failure
              liftIO $ putDispLn (Output.GuardDenied toolName reason)
              pure $ Left $ "Guard denied: " <> reason
            Guard.NeedsResolution (Guard.AskUser question) -> do
              -- User input is needed as a last resort
              liftIO $ putDispLn (Output.NeedsUserInput toolName question.questionText)
              pure $ Left $ "Guard needs user input: " <> question.questionText
            Guard.NeedsResolution (Guard.RunQueries _) -> do
              -- Resolution attempts exhausted
              liftIO $ putDispLn (Output.GuardDenied toolName "Could not establish required facts after resolution attempts")
              pure $ Left $ "Guard blocked: could not establish required facts"
            Guard.GuardAllowed -> do
              liftIO $ putDispLn (Output.GuardPass toolName)
              -- Execute the tool action (which can modify DB state via 'modify')
              actionResult <- runExceptT (tool.toolAction args)
              -- On success, collect facts produced by the tool
              case actionResult of
                Right _ -> do
                  producedFacts <- tool.toolFactsProduced args
                  #facts %= Facts.addFacts producedFacts
                Left _ -> pure ()
              pure actionResult

  let resultText = either ("ERROR: " <>) (\x -> x) result
  liftIO $ case result of
    Left err -> putDispLn (Output.ToolError err)
    Right obs -> putDispLn (Output.Observation obs)

  pure $ ToolMsg callId resultText

-- | Format a guard failure for display.
formatGuardFailure :: Guard.GuardFailure -> Text
formatGuardFailure = \case
  Guard.FactNotEstablished reason -> "Missing required fact: " <> reason
  Guard.ForbiddenFactPresent reason -> "Forbidden fact present: " <> reason
  Guard.PolicyDenied reason -> reason

--------------------------------------------------------------------------------
-- Main Agent Loop
--------------------------------------------------------------------------------

runAgent ::
  (Ord fact) =>
  AgentConfig ->
  Tool.Toolkit (AgentM db fact) fact ->
  db ->
  Facts.FactsDB fact ->
  [Message] ->
  Int ->
  Text ->
  IO (Text, db, Facts.FactsDB fact, [Message], Int)
runAgent config toolkit db initialFacts history turnCount userQuery = do
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
            facts = initialFacts,
            messages = UserMsg userQuery : history,
            turnCount = newTurnCount,
            iterationCount = 0
          }
  (response, finalState) <- runAgentM env initialState agentLoop
  pure (response, finalState.db, finalState.facts, finalState.messages, finalState.turnCount)

agentLoop :: (Ord fact) => AgentM db fact Text
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

handleResponse :: (Ord fact) => Chat.Message Text -> AgentM db fact Text
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
