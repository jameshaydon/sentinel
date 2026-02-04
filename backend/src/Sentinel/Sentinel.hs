-- | Sentinel: The middleware layer that guards tool calls.
--
-- Sentinel sits between the LLM Agent and the actual tool execution.
-- It evaluates guards, automatically invokes data tools to establish facts,
-- manages the fact store, and returns results to the agent.
--
-- The agent calls 'guardedCall' for every tool invocation, and Sentinel
-- handles all the complexity of guard resolution internally.
module Sentinel.Sentinel
  ( -- * Session Data
    SessionData (..),

    -- * Sentinel Result
    SentinelResult (..),

    -- * Sentinel State (unified mutable state)
    SentinelState (..),

    -- * Sentinel Environment
    SentinelEnv (..),
    newSentinelEnv,

    -- * Sentinel Monad
    SentinelM,
    runSentinelM,

    -- * Sentinel Interface
    Sentinel (..),

    -- * Context Operations
    setContextValue,
    getContextStore,
    formatContextForLLM,

    -- * Askable Fact Operations
    setAskableFact,
    getAskableStore,

    -- * Pending User Input Operations
    PendingUserInput (..),
    addPendingUserInput,
    getPendingUserInputs,
    clearPendingUserInput,
    clearAllPendingUserInputs,

    -- * Database Access
    getDb,
    modifyDb,

    -- * User Questions
    UserQuestion (..),

    -- * Proofs Found Flag
    setProofsFound,
    getProofsFound,

    -- * Pending User Input Queries
    findPendingContext,
    findPendingAskable,
    getPendingContextVars,
    getPendingAskables,

    -- * Verbosity / Debug
    Verbosity (..),
    setVerbosity,
    getVerbosity,

    -- * Event Emission
    emitEvent,

    -- * Re-exports from Event
    EventSink (..),
    UserInput (..),
    InputKind (..),
    InputMeta (..),
    consoleEventSink,
    consoleUserInput,
  )
where

import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Pre
import Sentinel.Context (ContextDecl (..), ContextDecls (..), ContextEstablishment (..), ContextStore, EstablishmentMethod (..), SeedSpec (..), emptyContextStore)
import Sentinel.Context qualified as Context
import Sentinel.Event (EventSink (..), InputKind (..), InputMeta (..), UserInput (..), consoleEventSink, consoleUserInput)
import Sentinel.Facts (AskableFactStore, BaseFactStore, HasFactStore (..), emptyAskableFactStore)
import Sentinel.Facts qualified as Facts
import Sentinel.Solver.Types (Scalar (..), UserInputType (..))
import Sentinel.Verbosity (Verbosity (..))

--------------------------------------------------------------------------------
-- User Questions
--------------------------------------------------------------------------------

-- | A question to ask the user when facts cannot be established via queries.
data UserQuestion = UserQuestion
  { -- | The type of input (context or askable)
    inputType :: UserInputType,
    -- | The name of the context variable or askable predicate
    inputName :: Text,
    -- | Arguments (empty for context, predicate args for askable)
    arguments :: [Scalar],
    -- | The question text to present to the user
    questionText :: Text,
    -- | Candidate values (for context variables)
    candidates :: [Scalar]
  }
  deriving stock (Show, Eq, Generic)


--------------------------------------------------------------------------------
-- Pending User Input
--------------------------------------------------------------------------------

-- | Unified information about a user input request that was asked and is awaiting response.
-- This replaces PendingAskableInfo with a unified structure for both context and askables.
data PendingUserInput = PendingUserInput
  { -- | The type of input (context or askable)
    pendingType :: UserInputType,
    -- | The name of the context variable or askable predicate
    pendingName :: Text,
    -- | Arguments (empty for context, predicate args for askable)
    pendingArguments :: [Scalar],
    -- | The human-readable question that was asked
    pendingQuestion :: Text,
    -- | Candidate values (for context variables)
    pendingCandidates :: [Scalar]
  }
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Session Data
--------------------------------------------------------------------------------

-- | Session data provided at startup (e.g., from authentication).
--
-- This data is used to pre-seed context variables that have a 'FromSession'
-- seed specification.
data SessionData = SessionData
  { -- | The authenticated user's ID (if available)
    userId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Sentinel Result
--------------------------------------------------------------------------------

-- | The result of a guarded tool call.
--
-- This is what the Sentinel returns to the Agent after processing a tool call.
data SentinelResult
  = -- | Tool executed successfully, with the result text
    Allowed Text
  | -- | Tool call was denied by guard, with the reason
    Denied Text
  | -- | Sentinel needs user input before it can proceed.
    --   The 'Text' field carries the formatted solver outcome for the LLM.
    AskUser UserQuestion Text
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Sentinel State (unified mutable state)
--------------------------------------------------------------------------------

-- | All mutable session state, kept in a single 'IORef'.
data SentinelState db = SentinelState
  { -- | The database (mutable for potential updates)
    db :: db,
    -- | The facts database (accumulated knowledge as BaseFacts)
    facts :: BaseFactStore,
    -- | The context store (context variables like booking_of_interest)
    contextStore :: ContextStore,
    -- | The askable fact store (user confirmations)
    askableStore :: AskableFactStore,
    -- | Pending user inputs awaiting response
    pendingUserInputs :: [PendingUserInput],
    -- | Whether the last solver run found any proofs (alongside blocks)
    proofsFound :: Bool,
    -- | Debug verbosity level (mutable for runtime adjustment)
    verbosity :: Verbosity
  }
  deriving stock (Generic)

--------------------------------------------------------------------------------
-- Sentinel Environment
--------------------------------------------------------------------------------

-- | The Sentinel's environment.
--
-- Contains a single 'IORef' to the unified 'SentinelState', plus I/O
-- abstractions for display events and user input.
--
-- Parameterized over @db@: The database type (e.g., AirlineDB).
data SentinelEnv db = SentinelEnv
  { -- | Single mutable state reference
    stateRef :: IORef (SentinelState db),
    -- | Output channel for display events
    eventSink :: EventSink,
    -- | Channel for side-session user input
    userInput :: UserInput
  }

-- | Create a new Sentinel environment with initial database, facts, and session data.
--
-- The 'SessionData' and 'ContextDecls' are used to pre-seed context variables.
-- For each context declaration with a 'FromSession' seed spec, if the corresponding
-- session field is present, the context variable is pre-populated.
newSentinelEnv ::
  db ->
  BaseFactStore ->
  SessionData ->
  ContextDecls ->
  Verbosity ->
  EventSink ->
  UserInput ->
  IO (SentinelEnv db)
newSentinelEnv initialDb initialFacts sessionData contextDecls initialVerbosity sink input = do
  -- Seed context from session data
  now <- getCurrentTime
  let seededContext = seedContextFromSession sessionData contextDecls now
      initialState =
        SentinelState
          { db = initialDb,
            facts = initialFacts,
            contextStore = seededContext,
            askableStore = emptyAskableFactStore,
            pendingUserInputs = [],
            proofsFound = False,
            verbosity = initialVerbosity
          }

  ref <- newIORef initialState

  pure
    SentinelEnv
      { stateRef = ref,
        eventSink = sink,
        userInput = input
      }

-- | Seed context variables from session data based on context declarations.
seedContextFromSession :: SessionData -> ContextDecls -> UTCTime -> ContextStore
seedContextFromSession sessionData (ContextDecls decls) timestamp =
  foldl' seedOne emptyContextStore (M.elems decls)
  where
    seedOne :: ContextStore -> ContextDecl -> ContextStore
    seedOne store decl = case decl.seedValue of
      Nothing -> store
      Just (Constant value) ->
        Context.setContext decl.name (mkEstablishment value) store
      Just (FromSession fieldName) ->
        case getSessionField fieldName sessionData of
          Nothing -> store
          Just value ->
            Context.setContext decl.name (mkEstablishment (ScStr value)) store

    mkEstablishment :: Scalar -> ContextEstablishment
    mkEstablishment value =
      ContextEstablishment
        { value = value,
          establishedVia = SystemSeeded,
          timestamp = timestamp
        }

    -- Get a field from session data by name
    getSessionField :: Text -> SessionData -> Maybe Text
    getSessionField "user_id" sess = sess.userId
    getSessionField _ _ = Nothing

--------------------------------------------------------------------------------
-- Sentinel Monad
--------------------------------------------------------------------------------

-- | The Sentinel monad.
--
-- This is the context in which Sentinel operations run. It has access to:
-- - The unified state (via IORef)
-- - The event sink (for display output)
-- - The user input channel (for side sessions)
-- - IO for executing tools
type SentinelM db = ReaderT (SentinelEnv db) IO

-- | Run a Sentinel computation.
runSentinelM :: SentinelEnv db -> SentinelM db a -> IO a
runSentinelM env action = runReaderT action env

--------------------------------------------------------------------------------
-- Sentinel Interface
--------------------------------------------------------------------------------

-- | The Sentinel interface for a specific domain.
--
-- This record bundles the operations needed to guard tool calls for a
-- particular domain (e.g., AirCanada). The implementation handles:
-- - Guard evaluation
-- - Automatic fact establishment via data tool calls
--
-- Example usage:
-- @
-- sentinel <- airCanadaSentinel config
-- result <- runSentinelM env (sentinel.guardedCall "InitiateRefund" args)
-- @
data Sentinel db = Sentinel
  { -- | Guard a tool call, returning the result or asking for user input.
    --
    -- This is the main entry point for the agent. It:
    -- 1. Evaluates the guard for the tool
    -- 2. Guards may execute data tools inline to establish facts
    -- 3. If guard allows, executes the tool and returns 'Allowed'
    -- 4. If guard denies, returns 'Denied'
    -- 5. If guard needs user input, returns 'AskUser'
    guardedCall :: Text -> Value -> SentinelM db SentinelResult,
    -- | Get a summary of current facts for the LLM context.
    summarizeFacts :: SentinelM db Text
  }

--------------------------------------------------------------------------------
-- Event Emission
--------------------------------------------------------------------------------

-- | Emit a display event through the environment's 'EventSink'.
emitEvent :: Doc Ann -> SentinelM db ()
emitEvent doc = do
  sink <- asks (.eventSink)
  liftIO $ sink.emit doc

--------------------------------------------------------------------------------
-- Fact Operations (HasFactStore instance)
--------------------------------------------------------------------------------

instance HasFactStore (SentinelM db) where
  addFact fact = do
    ref <- asks (.stateRef)
    liftIO $ modifyIORef' ref (over #facts (Facts.addBaseFact fact))
  addFacts newFacts = do
    ref <- asks (.stateRef)
    liftIO $ modifyIORef' ref (over #facts (Facts.addBaseFacts newFacts))
  getFactStore = do
    ref <- asks (.stateRef)
    liftIO $ (.facts) <$> readIORef ref

--------------------------------------------------------------------------------
-- Database Access
--------------------------------------------------------------------------------

-- | Get the current database state.
getDb :: SentinelM db db
getDb = do
  ref <- asks (.stateRef)
  liftIO $ (.db) <$> readIORef ref

-- | Modify the database state.
modifyDb :: (db -> db) -> SentinelM db ()
modifyDb f = do
  ref <- asks (.stateRef)
  liftIO $ modifyIORef' ref (over #db f)

--------------------------------------------------------------------------------
-- Context Operations
--------------------------------------------------------------------------------

-- | Set a context variable with a value (from user selection).
setContextValue :: Text -> Scalar -> [Scalar] -> SentinelM db ()
setContextValue slot value candidates = do
  ref <- asks (.stateRef)
  timestamp <- liftIO getCurrentTime
  let establishment =
        ContextEstablishment
          { value = value,
            establishedVia = UserSelection candidates,
            timestamp = timestamp
          }
  liftIO $ modifyIORef' ref (over #contextStore (Context.setContext slot establishment))

-- | Get the entire context store (for solver).
getContextStore :: SentinelM db ContextStore
getContextStore = do
  ref <- asks (.stateRef)
  liftIO $ (.contextStore) <$> readIORef ref

-- | Format context for inclusion in LLM messages.
--
-- Returns a text description of the current context that the LLM can use
-- to understand the session state.
formatContextForLLM :: ContextStore -> Text
formatContextForLLM store =
  let entries = M.toList store.established
   in if null entries
        then "No context established."
        else
          T.unlines
            [ "Current context:",
              T.unlines
                [ "  • " <> name <> " = " <> formatScalar est.value <> formatMethod est.establishedVia
                | (name, est) <- entries
                ]
            ]
  where
    formatScalar (ScStr t) = t
    formatScalar (ScNum n) = T.pack (show n)
    formatScalar (ScBool b) = if b then "true" else "false"
    formatScalar (ScExpr name []) = name
    formatScalar (ScExpr name args) = name <> "(" <> T.intercalate ", " (map formatScalar args) <> ")"

    formatMethod SystemSeeded = " (authenticated)"
    formatMethod (UserSelection _) = " (user selected)"
    formatMethod (DerivedFrom src) = " (from " <> src <> ")"

--------------------------------------------------------------------------------
-- Askable Fact Operations
--------------------------------------------------------------------------------

-- | Record a user confirmation for an askable predicate.
setAskableFact :: Text -> [Scalar] -> Bool -> SentinelM db ()
setAskableFact predName args confirmed = do
  ref <- asks (.stateRef)
  liftIO $ modifyIORef' ref (over #askableStore (Facts.addAskableFact predName args confirmed))

-- | Get the entire askable fact store (for solver).
getAskableStore :: SentinelM db AskableFactStore
getAskableStore = do
  ref <- asks (.stateRef)
  liftIO $ (.askableStore) <$> readIORef ref

--------------------------------------------------------------------------------
-- Pending User Input Operations (unified)
--------------------------------------------------------------------------------

-- | Add a pending user input awaiting response.
addPendingUserInput :: UserInputType -> Text -> [Scalar] -> Text -> [Scalar] -> SentinelM db ()
addPendingUserInput inputType inputName args questionText inputCandidates = do
  ref <- asks (.stateRef)
  let info =
        PendingUserInput
          { pendingType = inputType,
            pendingName = inputName,
            pendingArguments = args,
            pendingQuestion = questionText,
            pendingCandidates = inputCandidates
          }
  -- Only add if not already pending (prevent duplicates)
  liftIO $ modifyIORef' ref $ over #pendingUserInputs $ \existing ->
    if any (\p -> p.pendingType == inputType && p.pendingName == inputName && p.pendingArguments == args) existing
      then existing
      else info : existing

-- | Get all pending user inputs.
getPendingUserInputs :: SentinelM db [PendingUserInput]
getPendingUserInputs = do
  ref <- asks (.stateRef)
  liftIO $ (.pendingUserInputs) <$> readIORef ref

-- | Clear a pending user input by name (after it's been resolved).
clearPendingUserInput :: Text -> SentinelM db ()
clearPendingUserInput name = do
  ref <- asks (.stateRef)
  liftIO $ modifyIORef' ref (over #pendingUserInputs (filter (\p -> p.pendingName /= name)))

-- | Clear all pending user inputs (used before a query to get a fresh picture).
clearAllPendingUserInputs :: SentinelM db ()
clearAllPendingUserInputs = do
  ref <- asks (.stateRef)
  liftIO $ modifyIORef' ref (set #pendingUserInputs [])

-- | Find a pending context variable by name.
findPendingContext :: Text -> SentinelM db (Maybe PendingUserInput)
findPendingContext name = do
  pending <- getPendingUserInputs
  pure $ find (\p -> p.pendingType == ContextInput && p.pendingName == name) pending

-- | Find a pending askable by name.
findPendingAskable :: Text -> SentinelM db (Maybe PendingUserInput)
findPendingAskable name = do
  pending <- getPendingUserInputs
  pure $ find (\p -> p.pendingType == AskableInput && p.pendingName == name) pending

-- | Get all pending context variable names.
getPendingContextVars :: SentinelM db [Text]
getPendingContextVars = do
  pending <- getPendingUserInputs
  pure [p.pendingName | p <- pending, p.pendingType == ContextInput]

-- | Get all pending askables (predicate name, arguments).
getPendingAskables :: SentinelM db [(Text, [Scalar])]
getPendingAskables = do
  pending <- getPendingUserInputs
  pure [(p.pendingName, p.pendingArguments) | p <- pending, p.pendingType == AskableInput]

--------------------------------------------------------------------------------
-- Proofs Found Flag
--------------------------------------------------------------------------------

-- | Set whether the last solver run found proofs.
setProofsFound :: Bool -> SentinelM db ()
setProofsFound found = do
  ref <- asks (.stateRef)
  liftIO $ modifyIORef' ref (set #proofsFound found)

-- | Get whether the last solver run found proofs.
getProofsFound :: SentinelM db Bool
getProofsFound = do
  ref <- asks (.stateRef)
  liftIO $ (.proofsFound) <$> readIORef ref

--------------------------------------------------------------------------------
-- Verbosity / Debug Operations
--------------------------------------------------------------------------------

-- | Set the verbosity level.
setVerbosity :: Verbosity -> SentinelM db ()
setVerbosity level = do
  ref <- asks (.stateRef)
  liftIO $ modifyIORef' ref (set #verbosity level)

-- | Get the verbosity level.
getVerbosity :: SentinelM db Verbosity
getVerbosity = do
  ref <- asks (.stateRef)
  liftIO $ (.verbosity) <$> readIORef ref
