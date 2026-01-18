-- | Guard monad with LogicT for backtracking guard resolution.
-- Guards control whether tool calls are allowed, using facts from the FactsDB.
-- Guards can directly execute data tools to establish missing facts.
--
-- DESIGN: Guards have access to the Sentinel environment and can execute
-- data tools inline via 'establishFact'. This eliminates the need for
-- external resolution loops.
module Sentinel.Guard
  ( -- * Guard Monad
    GuardM,
    GuardEnv (..),
    GuardState (..),
    runGuard,

    -- * Guard Results
    GuardResult (..),
    GuardFailure (..),
    FailureReason (..),

    -- * Guard Primitives
    requireFact,
    requireFactMatching,
    forbidFact,
    forbidFactMatching,
    denyWith,
    failWith,
    getFacts,
    queryFacts,
    askUser,

    -- * Tool Execution
    establishFact,
    executeDataTool,
    liftSentinelM,

    -- * Running Guards
    evaluateGuard,
  )
where

import Control.Monad.Logic (LogicT, observeAllT)
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Aeson (Value)
import Data.IORef (readIORef)
import Data.Text qualified as T
import Pre
import Sentinel.Facts (FactsDB)
import Sentinel.Facts qualified as Facts
import Sentinel.Sentinel (SentinelEnv (..), SentinelM, UserQuestion (..), addFacts)

-- | Convert a Show value to Text.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A specific reason why a guard branch failed.
data FailureReason
  = -- | A required fact was not found
    MissingFact Text
  | -- | A forbidden fact was present
    ForbiddenFact Text
  | -- | Policy explicitly denied with reason
    ExplicitDenial Text
  | -- | A data tool query failed
    QueryFailed Text Text -- Tool name, error message
  deriving stock (Show, Eq, Generic)

-- | Why a guard failed - collects all failure reasons.
data GuardFailure = GuardFailure
  { -- | All reasons collected during guard evaluation
    reasons :: [FailureReason]
  }
  deriving stock (Show, Eq, Generic)

-- | Result of guard evaluation.
data GuardResult
  = -- | Guard passed, tool call is allowed
    GuardAllowed
  | -- | Guard failed with reason
    GuardDenied GuardFailure
  | -- | Guard needs user input before proceeding
    NeedsUserInput UserQuestion
  deriving stock (Show, Eq, Generic)

-- | Environment for guard evaluation.
--
-- Contains:
-- - Access to sentinel operations (facts, db)
-- - A callback to execute data tools by name (avoids circular import with Tool)
data GuardEnv db fact = GuardEnv
  { -- | The sentinel environment for db/facts access
    sentinelEnv :: SentinelEnv db fact,
    -- | Callback to execute a data tool by name.
    -- Returns (observation, producedFacts) on success.
    runDataTool :: Text -> Value -> SentinelM db fact (Either Text (Text, [fact]))
  }

-- | State accumulated during guard evaluation.
data GuardState fact = GuardState
  { -- | Questions that could be asked if facts can't be established
    pendingQuestions :: [UserQuestion],
    -- | All failure reasons encountered during evaluation
    failureReasons :: [FailureReason]
  }
  deriving stock (Generic)

-- | The guard monad.
--
-- LogicT provides backtracking for trying multiple resolution paths.
-- StateT accumulates pending questions and failure reasons.
-- ReaderT provides access to the guard environment (sentinel + tool execution).
type GuardM db fact = LogicT (StateT (GuardState fact) (ReaderT (GuardEnv db fact) IO))

--------------------------------------------------------------------------------
-- Running Guards
--------------------------------------------------------------------------------

-- | Run a guard computation and collect all solutions.
runGuard ::
  GuardEnv db fact ->
  GuardM db fact a ->
  IO ([a], GuardState fact)
runGuard env guardAction = do
  let initialState =
        GuardState
          { pendingQuestions = [],
            failureReasons = []
          }
  runReaderT (runStateT (observeAllT guardAction) initialState) env

-- | Evaluate a guard and produce a result.
evaluateGuard ::
  GuardEnv db fact ->
  GuardM db fact () ->
  IO GuardResult
evaluateGuard env guardAction = do
  (solutions, finalState) <- runGuard env guardAction
  pure $ case solutions of
    -- At least one solution succeeded
    (_ : _) -> GuardAllowed
    -- No solutions - check if we have pending user questions
    [] -> case finalState.pendingQuestions of
      (question : _) -> NeedsUserInput question
      [] -> GuardDenied (GuardFailure finalState.failureReasons)

--------------------------------------------------------------------------------
-- Sentinel Access
--------------------------------------------------------------------------------

-- | Lift a SentinelM operation into GuardM.
liftSentinelM :: SentinelM db fact a -> GuardM db fact a
liftSentinelM action = do
  env <- lift $ lift ask
  liftIO $ runReaderT action env.sentinelEnv

-- | Get the current facts from the sentinel.
getFacts :: GuardM db fact (FactsDB fact)
getFacts = do
  env <- lift $ lift ask
  liftIO $ readIORef env.sentinelEnv.facts

--------------------------------------------------------------------------------
-- Guard Primitives
--------------------------------------------------------------------------------

-- | Record a failure reason and fail the branch.
failWith :: FailureReason -> GuardM db fact a
failWith reason = do
  #failureReasons %= (reason :)
  empty

-- | Require a specific fact to be present.
requireFact :: (Ord fact, Show fact) => fact -> GuardM db fact ()
requireFact fact = do
  facts <- getFacts
  unless (Facts.hasFact fact facts) do
    failWith (MissingFact $ "Required: " <> tshow fact)

-- | Require a fact matching a predicate to be present.
-- Returns the matching fact if found.
requireFactMatching :: Text -> (fact -> Bool) -> GuardM db fact fact
requireFactMatching description predicate = do
  facts <- getFacts
  case Facts.queryFacts predicate facts of
    (f : _) -> pure f
    [] -> failWith (MissingFact description)

-- | Forbid a specific fact from being present.
forbidFact :: (Ord fact, Show fact) => fact -> GuardM db fact ()
forbidFact fact = do
  facts <- getFacts
  when (Facts.hasFact fact facts) do
    failWith (ForbiddenFact $ "Forbidden: " <> tshow fact)

-- | Forbid any fact matching a predicate.
forbidFactMatching :: (Show fact) => Text -> (fact -> Bool) -> GuardM db fact ()
forbidFactMatching description predicate = do
  facts <- getFacts
  case Facts.queryFacts predicate facts of
    (f : _) -> failWith (ForbiddenFact $ description <> ": " <> tshow f)
    [] -> pure ()

-- | Explicitly deny with a reason. This always fails the guard.
denyWith :: Text -> GuardM db fact a
denyWith reason = failWith (ExplicitDenial reason)

-- | Query facts matching a predicate.
queryFacts :: (fact -> Bool) -> GuardM db fact [fact]
queryFacts predicate = do
  facts <- getFacts
  pure (Facts.queryFacts predicate facts)

-- | Register a user question (for facts that can only be established by asking).
askUser :: UserQuestion -> GuardM db fact ()
askUser question = #pendingQuestions %= (question :)

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Execute a data tool by name and add its produced facts.
--
-- This is the core mechanism for guards to invoke data tools inline.
-- The tool is looked up and executed via the callback in GuardEnv.
executeDataTool :: (Ord fact) => Text -> Value -> GuardM db fact (Text, [fact])
executeDataTool toolName args = do
  env <- lift $ lift ask
  result <- liftIO $ runReaderT (env.runDataTool toolName args) env.sentinelEnv
  case result of
    Left err -> failWith (QueryFailed toolName err)
    Right (obs, facts) -> do
      -- Add the produced facts to the fact store
      liftSentinelM (addFacts facts)
      pure (obs, facts)

-- | Try to establish a fact by executing a data tool if needed.
--
-- If the fact is already present, succeeds immediately.
-- Otherwise, executes the specified data tool and checks if the fact
-- is now established.
--
-- Example:
-- @
-- establishFact (BookingExists ref) "RetrieveBooking" (object ["bookingRef" .= ref])
-- @
establishFact :: (Ord fact, Show fact) => fact -> Text -> Value -> GuardM db fact ()
establishFact targetFact toolName args = do
  facts <- getFacts
  if Facts.hasFact targetFact facts
    then pure () -- Fact already present
    else do
      -- Execute the data tool to try to establish the fact
      _ <- executeDataTool toolName args
      -- Check if the fact is now established
      newFacts <- getFacts
      unless (Facts.hasFact targetFact newFacts) $
        failWith (MissingFact $ "Tool " <> toolName <> " did not establish: " <> tshow targetFact)
