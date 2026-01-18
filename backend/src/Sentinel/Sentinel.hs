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

    -- * Fact Operations
    addFact,
    addFacts,
    getFacts,

    -- * Database Access
    getDb,
    modifyDb,

    -- * Re-exports
    UserQuestion (..),
  )
where

import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Pre
import Sentinel.Facts (FactsDB)
import Sentinel.Facts qualified as Facts
import Sentinel.Guard (UserQuestion (..))

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
-- This is parameterized over:
-- - @db@: The database type (e.g., AirlineDB)
-- - @fact@: The fact type (e.g., AirCanada.Fact)
data SentinelEnv db fact = SentinelEnv
  { -- | The database (mutable for potential updates)
    db :: IORef db,
    -- | The facts database (accumulated knowledge)
    facts :: IORef (FactsDB fact)
  }
  deriving stock (Generic)

-- | Create a new Sentinel environment with initial database and facts.
newSentinelEnv :: db -> FactsDB fact -> IO (SentinelEnv db fact)
newSentinelEnv initialDb initialFacts = do
  dbRef <- newIORef initialDb
  factsRef <- newIORef initialFacts
  pure
    SentinelEnv
      { db = dbRef,
        facts = factsRef
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
type SentinelM db fact = ReaderT (SentinelEnv db fact) IO

-- | Run a Sentinel computation.
runSentinelM :: SentinelEnv db fact -> SentinelM db fact a -> IO a
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
-- - Resolution loops
--
-- Example usage:
-- @
-- sentinel <- airCanadaSentinel config
-- result <- runSentinelM env (sentinel.guardedCall "InitiateRefund" args)
-- @
data Sentinel db fact = Sentinel
  { -- | Guard a tool call, returning the result or asking for user input.
    --
    -- This is the main entry point for the agent. It:
    -- 1. Evaluates the guard for the tool
    -- 2. If blocked on missing facts, tries to establish them via data tools
    -- 3. If still blocked and askable, returns 'AskUser'
    -- 4. If allowed, executes the tool and returns 'Allowed'
    -- 5. If denied, returns 'Denied'
    guardedCall :: Text -> Value -> SentinelM db fact SentinelResult,
    -- | Get a summary of current facts for the LLM context.
    summarizeFacts :: SentinelM db fact Text
  }

--------------------------------------------------------------------------------
-- Fact Operations
--------------------------------------------------------------------------------

-- | Add a single fact to the Sentinel's fact store.
addFact :: (Ord fact) => fact -> SentinelM db fact ()
addFact fact = do
  factsRef <- asks (.facts)
  liftIO $ modifyIORef' factsRef (Facts.addFact fact)

-- | Add multiple facts to the Sentinel's fact store.
addFacts :: (Ord fact) => [fact] -> SentinelM db fact ()
addFacts newFacts = do
  factsRef <- asks (.facts)
  liftIO $ modifyIORef' factsRef (Facts.addFacts newFacts)

-- | Get the current facts database.
getFacts :: SentinelM db fact (FactsDB fact)
getFacts = do
  factsRef <- asks (.facts)
  liftIO $ readIORef factsRef

--------------------------------------------------------------------------------
-- Database Access
--------------------------------------------------------------------------------

-- | Get the current database state.
getDb :: SentinelM db fact db
getDb = do
  dbRef <- asks (.db)
  liftIO $ readIORef dbRef

-- | Modify the database state.
modifyDb :: (db -> db) -> SentinelM db fact ()
modifyDb f = do
  dbRef <- asks (.db)
  liftIO $ modifyIORef' dbRef f
