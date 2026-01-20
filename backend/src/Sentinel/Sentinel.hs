-- | Sentinel: The middleware layer that guards tool calls.
--
-- Sentinel sits between the LLM Agent and the actual tool execution.
-- It evaluates guards, automatically invokes data tools to establish facts,
-- manages the fact store, and returns results to the agent.
--
-- The agent calls 'guardedCall' for every tool invocation, and Sentinel
-- handles all the complexity of guard resolution internally.
module Sentinel.Sentinel
  ( -- * Sentinel Result
    SentinelResult (..),

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

    -- * Askable Fact Operations
    setAskableFact,
    getAskableStore,

    -- * Database Access
    getDb,
    modifyDb,

    -- * User Questions
    UserQuestion (..),
  )
where

import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Time (getCurrentTime)
import Pre
import Sentinel.Context (ContextEstablishment (..), ContextStore, EstablishmentMethod (..), emptyContextStore)
import Sentinel.Context qualified as Context
import Sentinel.Facts (AskableFactStore, BaseFactStore, HasFactStore (..), emptyAskableFactStore)
import Sentinel.Facts qualified as Facts
import Sentinel.Solver.Types (Scalar)

--------------------------------------------------------------------------------
-- User Questions
--------------------------------------------------------------------------------

-- | A question to ask the user when facts cannot be established via queries.
data UserQuestion = UserQuestion
  { -- | The question text to present to the user
    questionText :: Text,
    -- | Description of what fact we're trying to establish
    factDescription :: Text
  }
  deriving stock (Show, Eq, Generic)

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
  | -- | Sentinel needs user input before it can proceed
    AskUser UserQuestion
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Sentinel Environment
--------------------------------------------------------------------------------

-- | The Sentinel's environment, containing mutable state.
--
-- Parameterized over @db@: The database type (e.g., AirlineDB).
-- Facts are stored as 'BaseFact' in a 'BaseFactStore'.
data SentinelEnv db = SentinelEnv
  { -- | The database (mutable for potential updates)
    db :: IORef db,
    -- | The facts database (accumulated knowledge as BaseFacts)
    facts :: IORef BaseFactStore,
    -- | The context store (context variables like booking_of_interest)
    contextStore :: IORef ContextStore,
    -- | The askable fact store (user confirmations)
    askableStore :: IORef AskableFactStore
  }
  deriving stock (Generic)

-- | Create a new Sentinel environment with initial database and facts.
newSentinelEnv :: db -> BaseFactStore -> IO (SentinelEnv db)
newSentinelEnv initialDb initialFacts = do
  dbRef <- newIORef initialDb
  factsRef <- newIORef initialFacts
  contextRef <- newIORef emptyContextStore
  askableRef <- newIORef emptyAskableFactStore
  pure
    SentinelEnv
      { db = dbRef,
        facts = factsRef,
        contextStore = contextRef,
        askableStore = askableRef
      }

--------------------------------------------------------------------------------
-- Sentinel Monad
--------------------------------------------------------------------------------

-- | The Sentinel monad.
--
-- This is the context in which Sentinel operations run. It has access to:
-- - The database (via IORef)
-- - The facts database (via IORef)
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
-- Fact Operations (HasFactStore instance)
--------------------------------------------------------------------------------

instance HasFactStore (SentinelM db) where
  addFact fact = do
    factsRef <- asks (.facts)
    liftIO $ modifyIORef' factsRef (Facts.addBaseFact fact)
  addFacts newFacts = do
    factsRef <- asks (.facts)
    liftIO $ modifyIORef' factsRef (Facts.addBaseFacts newFacts)
  getFactStore = do
    factsRef <- asks (.facts)
    liftIO $ readIORef factsRef

--------------------------------------------------------------------------------
-- Database Access
--------------------------------------------------------------------------------

-- | Get the current database state.
getDb :: SentinelM db db
getDb = do
  dbRef <- asks (.db)
  liftIO $ readIORef dbRef

-- | Modify the database state.
modifyDb :: (db -> db) -> SentinelM db ()
modifyDb f = do
  dbRef <- asks (.db)
  liftIO $ modifyIORef' dbRef f

--------------------------------------------------------------------------------
-- Context Operations
--------------------------------------------------------------------------------

-- | Set a context variable with a value (from user selection).
setContextValue :: Text -> Scalar -> [Scalar] -> SentinelM db ()
setContextValue slot value candidates = do
  ctxRef <- asks (.contextStore)
  timestamp <- liftIO getCurrentTime
  let establishment =
        ContextEstablishment
          { value = value,
            establishedVia = UserSelection candidates,
            timestamp = timestamp
          }
  liftIO $ modifyIORef' ctxRef (Context.setContext slot establishment)

-- | Get the entire context store (for solver).
getContextStore :: SentinelM db ContextStore
getContextStore = do
  ctxRef <- asks (.contextStore)
  liftIO $ readIORef ctxRef

--------------------------------------------------------------------------------
-- Askable Fact Operations
--------------------------------------------------------------------------------

-- | Record a user confirmation for an askable predicate.
setAskableFact :: Text -> [Scalar] -> Bool -> SentinelM db ()
setAskableFact predName args confirmed = do
  askRef <- asks (.askableStore)
  liftIO $ modifyIORef' askRef (Facts.addAskableFact predName args confirmed)

-- | Get the entire askable fact store (for solver).
getAskableStore :: SentinelM db AskableFactStore
getAskableStore = do
  askRef <- asks (.askableStore)
  liftIO $ readIORef askRef
