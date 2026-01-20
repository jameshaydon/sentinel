-- | Facts database infrastructure for the Sentinel solver.
--
-- This module provides 'BaseFactStore' for storing solver facts as 'BaseFact' values.
-- The fact store is keyed by predicate name for efficient lookup.
module Sentinel.Facts
  ( -- * Fact Store Operations (Typeclass)
    HasFactStore (..),

    -- * Evidence
    Evidence (..),

    -- * Base Fact Store
    BaseFactStore (..),
    emptyBaseFactStore,
    addBaseFact,
    addBaseFacts,
    lookupBaseFacts,
    allBaseFacts,

    -- * Askable Fact Store
    AskableFactStore (..),
    emptyAskableFactStore,
    addAskableFact,
    lookupAskableFact,
    allAskableFacts,
  )
where

import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Pre
import Sentinel.Solver.Types (BaseFact (..), Scalar)

--------------------------------------------------------------------------------
-- Fact Store Typeclass
--------------------------------------------------------------------------------

-- | Typeclass for monads that have access to a base fact store.
class (Monad m) => HasFactStore m where
  -- | Add a single fact to the store.
  addFact :: BaseFact -> m ()

  -- | Add multiple facts to the store.
  addFacts :: [BaseFact] -> m ()
  addFacts = traverse_ addFact

  -- | Get the entire fact store.
  getFactStore :: m BaseFactStore

--------------------------------------------------------------------------------
-- Evidence
--------------------------------------------------------------------------------

-- | Evidence level for how a fact was established.
-- Higher constructors represent stronger evidence.
data Evidence
  = -- | User claimed this fact (lowest trust)
    UserClaim
  | -- | Verified via documentation
    DocumentVerified
  | -- | Verified via database lookup (highest trust)
    DatabaseVerified
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Base Fact Store
--------------------------------------------------------------------------------

-- | Store for solver BaseFacts.
--
-- BaseFacts are stored in a map keyed by predicate name for efficient lookup.
-- Each predicate name maps to a set of facts with that predicate.
newtype BaseFactStore = BaseFactStore
  { factsByPredicate :: Map Text (Set BaseFact)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty base fact store.
emptyBaseFactStore :: BaseFactStore
emptyBaseFactStore = BaseFactStore M.empty

-- | Add a single base fact to the store.
addBaseFact :: BaseFact -> BaseFactStore -> BaseFactStore
addBaseFact fact (BaseFactStore m) =
  BaseFactStore $
    M.insertWith Set.union fact.predicateName (Set.singleton fact) m

-- | Add multiple base facts to the store.
addBaseFacts :: [BaseFact] -> BaseFactStore -> BaseFactStore
addBaseFacts facts store = foldl' (flip addBaseFact) store facts

-- | Look up all facts for a given predicate name.
lookupBaseFacts :: Text -> BaseFactStore -> [BaseFact]
lookupBaseFacts predName (BaseFactStore m) =
  maybe [] Set.toList (M.lookup predName m)

-- | Get all base facts.
allBaseFacts :: BaseFactStore -> [BaseFact]
allBaseFacts (BaseFactStore m) = concatMap Set.toList (M.elems m)

--------------------------------------------------------------------------------
-- Askable Fact Store
--------------------------------------------------------------------------------

-- | Store for askable facts (user-confirmed predicates).
--
-- Askable facts are keyed by (predicate name, arguments) for exact lookup.
-- The Bool value indicates whether the user confirmed (True) or denied (False).
newtype AskableFactStore = AskableFactStore
  { confirmedAskables :: Map (Text, [Scalar]) Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty askable fact store.
emptyAskableFactStore :: AskableFactStore
emptyAskableFactStore = AskableFactStore M.empty

-- | Add an askable fact (user confirmation).
addAskableFact :: Text -> [Scalar] -> Bool -> AskableFactStore -> AskableFactStore
addAskableFact predName args confirmed (AskableFactStore m) =
  AskableFactStore $ M.insert (predName, args) confirmed m

-- | Look up whether an askable fact has been confirmed.
-- Returns Nothing if not yet asked, Just True if confirmed, Just False if denied.
lookupAskableFact :: Text -> [Scalar] -> AskableFactStore -> Maybe Bool
lookupAskableFact predName args (AskableFactStore m) =
  M.lookup (predName, args) m

-- | Get all askable facts as (predicate, args, confirmed) tuples.
allAskableFacts :: AskableFactStore -> [(Text, [Scalar], Bool)]
allAskableFacts (AskableFactStore m) =
  [(pred', args, confirmed) | ((pred', args), confirmed) <- M.toList m]
