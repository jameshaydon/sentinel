module Examples.Passport.Types
  ( PersonId,
    PassportDB (..),
    motherOf,
    fatherOf,
  )
where

import Sentinel.Solver.Types (Scalar (..))

-- | A person identifier as a compound term.
--
-- Examples: @ScStr "Romi Haydon"@, @ScExpr "mother" [ScStr "Romi Haydon"]@
type PersonId = Scalar

-- | Empty database — all facts come from askables.
data PassportDB = PassportDB

-- | Compute the mother's person ID.
motherOf :: PersonId -> PersonId
motherOf p = ScExpr "mother" [p]

-- | Compute the father's person ID.
fatherOf :: PersonId -> PersonId
fatherOf p = ScExpr "father" [p]
