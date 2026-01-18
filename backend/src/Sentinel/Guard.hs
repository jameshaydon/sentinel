-- | Guard monad with LogicT for backtracking guard resolution.
-- Guards control whether tool calls are allowed, using facts from the FactsDB.
-- When facts are missing, guards can request queries or user questions.
--
-- DESIGN: Guards only access facts, never the database directly. If a guard
-- needs data from the database, it should request a query via 'requestQuery'.
-- This ensures clean separation: facts are the "knowledge base", guards are
-- the "policy engine", and the agent is the "executor".
module Sentinel.Guard
  ( -- * Guard Monad
    GuardM,
    GuardState (..),
    runGuard,

    -- * Guard Results
    GuardResult (..),
    GuardFailure (..),
    FailureReason (..),
    Resolution (..),
    PendingQuery (..),
    UserQuestion (..),

    -- * Guard Primitives
    requireFact,
    requireFactMatching,
    forbidFact,
    forbidFactMatching,
    denyWith,
    getFacts,
    queryFacts,
    requestQuery,
    askUser,

    -- * Combinators
    tryEstablishFact,

    -- * Running Guards
    evaluateGuard,
  )
where

import Control.Monad.Logic (LogicT, observeAllT)
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Aeson (Value)
import Data.Text qualified as T
import Pre
import Sentinel.Facts (FactsDB)
import Sentinel.Facts qualified as Facts

-- | Convert a Show value to Text.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A pending query that could establish missing facts.
data PendingQuery = PendingQuery
  { -- | Name of the tool to call
    queryToolName :: Text,
    -- | Arguments to pass to the tool
    queryArgs :: Value,
    -- | Description of what facts this query would establish
    queryDescription :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | A question to ask the user when facts cannot be established via queries.
data UserQuestion = UserQuestion
  { -- | The question text to present to the user
    questionText :: Text,
    -- | Description of what fact we're trying to establish
    factDescription :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | How to resolve a blocked guard.
data Resolution
  = -- | Run these queries to try to establish missing facts
    RunQueries (NonEmpty PendingQuery)
  | -- | Ask the user this question (last resort)
    AskUser UserQuestion
  deriving stock (Show, Eq, Generic)

-- | A specific reason why a guard branch failed.
data FailureReason
  = -- | A required fact was not found
    MissingFact Text
  | -- | A forbidden fact was present
    ForbiddenFact Text
  | -- | Policy explicitly denied with reason
    ExplicitDenial Text
  | -- | A query was attempted but failed
    QueryFailed Text Text -- ^ Tool name, error message
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
  | -- | Guard blocked, needs resolution before proceeding
    NeedsResolution Resolution
  deriving stock (Show, Eq, Generic)

-- | State accumulated during guard evaluation.
data GuardState fact = GuardState
  { -- | Current facts (read from this)
    currentFacts :: FactsDB fact,
    -- | Queries that could be run to establish missing facts
    pendingQueries :: [PendingQuery],
    -- | Questions that could be asked if queries don't work
    pendingQuestions :: [UserQuestion],
    -- | All failure reasons encountered during evaluation
    failureReasons :: [FailureReason]
  }
  deriving stock (Generic)

-- | The guard monad.
-- LogicT provides backtracking for trying multiple resolution paths.
-- StateT accumulates pending queries and questions.
-- Guards only access facts, not the database - keeping policy separate from data access.
type GuardM fact = LogicT (StateT (GuardState fact) IO)

--------------------------------------------------------------------------------
-- Running Guards
--------------------------------------------------------------------------------

-- | Run a guard computation and collect all solutions.
runGuard ::
  FactsDB fact ->
  GuardM fact a ->
  IO ([a], GuardState fact)
runGuard facts guardAction = do
  let initialState =
        GuardState
          { currentFacts = facts,
            pendingQueries = [],
            pendingQuestions = [],
            failureReasons = []
          }
  runStateT (observeAllT guardAction) initialState

-- | Evaluate a guard and produce a result.
-- This runs the guard logic and interprets the outcome.
evaluateGuard ::
  FactsDB fact ->
  GuardM fact () ->
  IO GuardResult
evaluateGuard facts guardAction = do
  (solutions, finalState) <- runGuard facts guardAction
  pure $ case solutions of
    -- At least one solution succeeded
    (_ : _) -> GuardAllowed
    -- No solutions - check if we have pending work
    [] -> case finalState.pendingQueries of
      (q : qs) -> NeedsResolution (RunQueries (q :| qs))
      [] -> case finalState.pendingQuestions of
        (question : _) -> NeedsResolution (AskUser question)
        [] -> GuardDenied (GuardFailure finalState.failureReasons)

--------------------------------------------------------------------------------
-- Guard Primitives
--------------------------------------------------------------------------------

-- | Record a failure reason and fail the branch.
failWith :: FailureReason -> GuardM fact a
failWith reason = do
  #failureReasons %= (reason :)
  empty

-- | Require a specific fact to be present.
-- If the fact is missing, records the reason and fails the branch.
requireFact :: (Ord fact, Show fact) => fact -> GuardM fact ()
requireFact fact = do
  facts <- use (#currentFacts)
  unless (Facts.hasFact fact facts) do
    failWith (MissingFact $ "Required: " <> tshow fact)

-- | Require a fact matching a predicate to be present.
-- Returns the matching fact if found.
requireFactMatching :: Text -> (fact -> Bool) -> GuardM fact fact
requireFactMatching description predicate = do
  facts <- use (#currentFacts)
  case Facts.queryFacts predicate facts of
    (f : _) -> pure f
    [] -> failWith (MissingFact description)

-- | Forbid a specific fact from being present.
-- If the fact is present, records the reason and fails the branch.
forbidFact :: (Ord fact, Show fact) => fact -> GuardM fact ()
forbidFact fact = do
  facts <- use (#currentFacts)
  when (Facts.hasFact fact facts) do
    failWith (ForbiddenFact $ "Forbidden: " <> tshow fact)

-- | Forbid any fact matching a predicate.
-- If a matching fact is present, records the reason and fails the branch.
forbidFactMatching :: (Show fact) => Text -> (fact -> Bool) -> GuardM fact ()
forbidFactMatching description predicate = do
  facts <- use (#currentFacts)
  case Facts.queryFacts predicate facts of
    (f : _) -> failWith (ForbiddenFact $ description <> ": " <> tshow f)
    [] -> pure ()

-- | Explicitly deny with a reason. This always fails the guard.
-- The reason is recorded and will be included in the guard failure message.
denyWith :: Text -> GuardM fact a
denyWith reason = failWith (ExplicitDenial reason)

-- | Get all current facts.
getFacts :: GuardM fact (FactsDB fact)
getFacts = use #currentFacts

-- | Query facts matching a predicate.
queryFacts :: (fact -> Bool) -> GuardM fact [fact]
queryFacts predicate = do
  facts <- use #currentFacts
  pure (Facts.queryFacts predicate facts)

-- | Register a query that could establish missing facts.
-- This doesn't run the query immediately - it records it for the resolution phase.
requestQuery :: PendingQuery -> GuardM fact ()
requestQuery query = #pendingQueries %= (query :)

-- | Register a user question (last resort for establishing facts).
askUser :: UserQuestion -> GuardM fact ()
askUser question = #pendingQuestions %= (question :)

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

-- | Try to require a fact, and if missing, register a query that could establish it.
-- This is the key combinator for "auto-resolution" - the guard fails but records
-- what query could be run to make it succeed.
--
-- Example:
-- @
-- tryEstablishFact
--   (BookingExists ref)
--   (PendingQuery "RetrieveBooking" (object ["bookingRef" .= ref]) "Get booking details")
-- @
tryEstablishFact :: (Ord fact) => fact -> PendingQuery -> GuardM fact ()
tryEstablishFact fact query = do
  facts <- use #currentFacts
  if Facts.hasFact fact facts
    then pure () -- Fact already present, succeed
    else do
      -- Fact missing - register the query and fail this branch
      requestQuery query
      empty
