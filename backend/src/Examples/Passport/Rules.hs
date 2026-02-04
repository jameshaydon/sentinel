-- | UK British citizenship eligibility rules translated to the Sentinel solver.
--
-- Reference implementation: @examples/uk-passport/old.hs@
--
-- All facts are established through askables (user questions). Recursion is
-- naturally bounded by askable blocking: each solver run explores one level
-- deeper than previously answered questions.
module Examples.Passport.Rules
  ( brit,
  )
where

import Examples.Passport.Types (PersonId, fatherOf, motherOf)
import Pre
import Sentinel.Solver.Combinators (SolverM, andAll, askable, ifThenElse, oneOf, orElse, withRule)
import Sentinel.Solver.Types (Proof (..))

-- | Is a person British?
--
-- First checks if they might be British at all (via askable), then tries
-- by-birth or naturalisation paths.
brit :: PersonId -> SolverM Proof
brit person = withRule "brit" [person] do
  -- Pre-check: might this person be British?
  isBritProof <- askable "possibly_british" [person]
  -- Try by birth or naturalisation (explore both paths)
  proof <- oneOf
    [ byBirth person
    , askable "naturalised" [person]
    ]
  pure $ RuleApplied "brit" [person] [isBritProof, proof]

-- | British by birth: born in UK or born abroad.
byBirth :: PersonId -> SolverM Proof
byBirth person = withRule "by_birth" [person] do
  ifThenElse
    (askable "born_in_uk" [person])
    (britBornInUk person)
    (britBornAbroad person)

-- | UK-born citizenship: pre-1983 automatic, or via parent.
britBornInUk :: PersonId -> SolverM Proof
britBornInUk person = withRule "born_in_uk" [person] do
  askable "born_before_1983" [person]
    `orElse` britBornInUkViaParent person

-- | UK-born citizenship via parent (post-1983 births).
britBornInUkViaParent :: PersonId -> SolverM Proof
britBornInUkViaParent person = withRule "born_in_uk_via_parent" [person] do
  viaParent person \parent ->
    brit parent `orElse` settled parent

-- | Establish citizenship via a parent's status.
--
-- Handles different rules for maternal vs paternal citizenship:
-- - Maternal: always available
-- - Paternal: automatic after 2006, requires marriage before 2006
viaParent :: PersonId -> (PersonId -> SolverM Proof) -> SolverM Proof
viaParent person cond = oneOf [viaMother, viaFather]
  where
    mother = motherOf person
    father = fatherOf person
    viaMother = withRule "via_mother" [] do
      proof <- cond mother
      pure $ RuleApplied "via_mother" [] [proof]
    viaFather = withRule "via_father" [] do
      ifThenElse
        (askable "born_after_2006" [person])
        (do proof <- cond father; pure $ RuleApplied "via_father" [] [proof])
        (do
          marriageProof <- married father mother
          parentProof <- cond father
          pure $ RuleApplied "via_father_with_marriage" [] [marriageProof, parentProof]
        )

-- | British citizenship for those born abroad.
britBornAbroad :: PersonId -> SolverM Proof
britBornAbroad person = withRule "born_abroad" [person] do
  viaParent person \parent ->
    britOtbd parent
      `orElse` (do
        proof <- andAll [brit parent, askable "years_3_living_in_uk" [parent]]
        pure $ RuleApplied "brit_with_residency" [] [proof]
      )

-- | British otherwise than by descent (BOTD).
britOtbd :: PersonId -> SolverM Proof
britOtbd person = withRule "brit_otbd" [person] do
  isBritOtbdProof <- askable "possibly_brit_otbd" [person]
  proof <-
    askable "naturalised" [person]
      `orElse` britOtbdUkBorn
      `orElse` bornCrownService person
  pure $ RuleApplied "brit_otbd" [person] [isBritOtbdProof, proof]
  where
    britOtbdUkBorn = do
      proof <- andAll
        [ askable "born_in_uk" [person],
          britBornInUk person
        ]
      pure $ RuleApplied "brit_otbd_uk_born" [] [proof]

-- | Born in Crown service.
bornCrownService :: PersonId -> SolverM Proof
bornCrownService person = withRule "crown_service" [person] do
  viaParent person \parent -> do
    proof <- andAll
      [ askable "crown_service" [parent],
        brit parent
      ]
    pure $ RuleApplied "born_crown_service" [] [proof]

-- | Establish that two people were married at time of birth.
married :: PersonId -> PersonId -> SolverM Proof
married p q = askable "married" [p, q]

-- | Establish that a person had settled status.
settled :: PersonId -> SolverM Proof
settled person = askable "settled" [person]


