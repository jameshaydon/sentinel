-- | Generic facts database infrastructure.
-- The FactsDB stores accumulated knowledge during conversation.
-- Facts are parameterized over a domain-specific fact type.
module Sentinel.Facts
  ( -- * Evidence
    Evidence (..),

    -- * Facts Database
    FactsDB (..),
    emptyFacts,
    addFact,
    addFacts,
    hasFact,
    allFacts,
    queryFacts,
  )
where

import Data.Set qualified as Set
import Pre

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

-- | Facts store with efficient lookup.
-- Parameterized over the fact type 'f'.
newtype FactsDB f = FactsDB
  { facts :: Set f
  }
  deriving stock (Show, Eq, Generic)

-- | Empty facts database.
emptyFacts :: FactsDB f
emptyFacts = FactsDB Set.empty

-- | Add a single fact to the database.
addFact :: (Ord f) => f -> FactsDB f -> FactsDB f
addFact fact (FactsDB fs) = FactsDB (Set.insert fact fs)

-- | Add multiple facts to the database.
addFacts :: (Ord f) => [f] -> FactsDB f -> FactsDB f
addFacts newFacts (FactsDB fs) = FactsDB (fs <> Set.fromList newFacts)

-- | Check if a specific fact exists.
hasFact :: (Ord f) => f -> FactsDB f -> Bool
hasFact fact (FactsDB fs) = Set.member fact fs

-- | Get all facts as a list.
allFacts :: FactsDB f -> [f]
allFacts (FactsDB fs) = Set.toList fs

-- | Query facts matching a predicate.
queryFacts :: (f -> Bool) -> FactsDB f -> [f]
queryFacts p (FactsDB fs) = filter p (Set.toList fs)
