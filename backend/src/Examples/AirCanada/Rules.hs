-- | Refund policy rules for the Air Canada example.
--
-- These rules encode the Air Canada refund policy as solver combinators.
-- The solver evaluates these rules by:
-- 1. Querying predicates (which may invoke tools to establish facts)
-- 2. Backtracking through alternatives when paths fail
-- 3. Blocking on askable predicates that need user confirmation
-- 4. Producing proof traces for audit
--
-- === Rule Hierarchy
--
-- @
-- eligible_for_refund(Booking, Type, Reason)
--   ├── airline_at_fault(Booking)      → full refund, "airline_fault"
--   ├── eu261_eligible(Booking)        → full refund, "eu261"
--   ├── fare_allows_refund(Booking)    → varies by fare class
--   └── bereavement_eligible(Booking)  → partial refund, "bereavement"
-- @
module Examples.AirCanada.Rules
  ( -- * Main Eligibility Rule
    eligibleForRefund,

    -- * Component Rules
    airlineAtFault,
    eu261Eligible,
    fareAllowsRefund,
    bereavementEligible,
    medicalBasicEligible,

    -- * Refund Types
    RefundType (..),
    RefundReason (..),
  )
where

import Data.Map.Strict qualified as M
import Pre
import Sentinel.Solver.Combinators
  ( SolverM,
    andAll,
    askable,
    contextVar,
    oneOf,
    queryPredicate,
    require,
    withReason,
    withRule,
  )
import Sentinel.Solver.Types (BaseFact (..), Proof, Scalar (..), SolverSuccess (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Type of refund outcome.
data RefundType
  = FullRefund
  | PartialRefund
  | VoucherRefund
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Reason for refund eligibility.
data RefundReason
  = AirlineFault
  | EU261Regulation
  | FlexibleFare
  | StandardFare
  | Bereavement
  | MedicalBasic
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

refundTypeToText :: RefundType -> Text
refundTypeToText = \case
  FullRefund -> "full"
  PartialRefund -> "partial"
  VoucherRefund -> "voucher"

refundReasonToText :: RefundReason -> Text
refundReasonToText = \case
  AirlineFault -> "airline_fault"
  EU261Regulation -> "eu261"
  FlexibleFare -> "flexible_fare"
  StandardFare -> "standard_fare"
  Bereavement -> "bereavement"
  MedicalBasic -> "medical_basic"

--------------------------------------------------------------------------------
-- Main Eligibility Rule
--------------------------------------------------------------------------------

-- | Check if a booking is eligible for a refund.
--
-- This is the main entry point for refund eligibility queries. It tries
-- multiple paths in order of preference:
--
-- 1. Airline fault (cancelled or delayed flight) → full refund
-- 2. EU261 regulation (EU departure + significant delay) → full refund
-- 3. Flexible fare class → full refund
-- 4. Standard fare class (with advance notice) → partial refund
-- 5. Bereavement (with documentation) → partial refund
-- 6. Medical emergency for Basic Economy → voucher only
--
-- The rule uses the @booking_of_interest@ context variable, which must be
-- established before this rule can be evaluated.
--
-- ==== Example
--
-- @
-- -- Query eligibility for the current booking of interest
-- result <- runSolver env state eligibleForRefund
-- case result of
--   Success successes -> handleRefundOptions successes
--   BlockedOnContext block -> askUserToSelectBooking block
--   BlockedOnAskable block -> askUserConfirmation block
--   Failure paths -> explainWhyNotEligible paths
-- @
eligibleForRefund :: SolverM SolverSuccess
eligibleForRefund = do
  -- Get the booking from context
  booking <- contextVar "booking_of_interest"

  -- Try each path in priority order
  oneOf
    [ -- Path 1: Airline is at fault (cancelled or delayed)
      withRule "airline_fault_path" $ do
        withReason "airline_fault" $ do
          proof <- airlineAtFault booking
          pure $
            SolverSuccess
              { bindings =
                  M.fromList
                    [ ("RefundType", ScStr (refundTypeToText FullRefund)),
                      ("Reason", ScStr (refundReasonToText AirlineFault))
                    ],
                proof = proof,
                reason = "Flight was cancelled or significantly delayed by the airline"
              },
      -- Path 2: EU261 regulation applies
      withRule "eu261_path" $ do
        withReason "eu261" $ do
          proof <- eu261Eligible booking
          pure $
            SolverSuccess
              { bindings =
                  M.fromList
                    [ ("RefundType", ScStr (refundTypeToText FullRefund)),
                      ("Reason", ScStr (refundReasonToText EU261Regulation))
                    ],
                proof = proof,
                reason = "EU261 regulation applies (flight from EU with significant delay)"
              },
      -- Path 3: Flexible fare allows refund
      withRule "flexible_fare_path" $ do
        withReason "flexible_fare" $ do
          proof <- fareAllowsRefund booking "flexible" FullRefund
          pure $
            SolverSuccess
              { bindings =
                  M.fromList
                    [ ("RefundType", ScStr (refundTypeToText FullRefund)),
                      ("Reason", ScStr (refundReasonToText FlexibleFare))
                    ],
                proof = proof,
                reason = "Flexible fare class allows voluntary refund"
              },
      -- Path 4: Standard fare with advance notice
      withRule "standard_fare_path" $ do
        withReason "standard_fare" $ do
          proof <- fareAllowsRefund booking "standard" PartialRefund
          pure $
            SolverSuccess
              { bindings =
                  M.fromList
                    [ ("RefundType", ScStr (refundTypeToText PartialRefund)),
                      ("Reason", ScStr (refundReasonToText StandardFare))
                    ],
                proof = proof,
                reason = "Standard fare with cancellation penalty"
              },
      -- Path 5: Bereavement exception
      withRule "bereavement_path" $ do
        withReason "bereavement" $ do
          proof <- bereavementEligible booking
          pure $
            SolverSuccess
              { bindings =
                  M.fromList
                    [ ("RefundType", ScStr (refundTypeToText PartialRefund)),
                      ("Reason", ScStr (refundReasonToText Bereavement))
                    ],
                proof = proof,
                reason = "Bereavement exception with required documentation"
              },
      -- Path 6: Medical exception for Basic Economy (voucher only)
      withRule "medical_basic_path" $ do
        withReason "medical_basic" $ do
          proof <- medicalBasicEligible booking
          pure $
            SolverSuccess
              { bindings =
                  M.fromList
                    [ ("RefundType", ScStr (refundTypeToText VoucherRefund)),
                      ("Reason", ScStr (refundReasonToText MedicalBasic))
                    ],
                proof = proof,
                reason = "Medical emergency for Basic Economy ticket (voucher only)"
              }
    ]

--------------------------------------------------------------------------------
-- Component Rules
--------------------------------------------------------------------------------

-- | Check if the airline is at fault for the booking's flight.
--
-- The airline is considered at fault if the flight is:
-- - Cancelled
-- - Delayed (any delay status qualifies)
--
-- Note: The current implementation treats any "Delayed" status as qualifying.
-- A more sophisticated version would check flight_delay_minutes > 180.
airlineAtFault :: Scalar -> SolverM Proof
airlineAtFault booking =
  withRule "airline_at_fault" $
    oneOf
      [ -- Flight is cancelled
        withRule "flight_cancelled" $ do
          -- Get the flight for this booking
          flightFact <- queryPredicate "booking_flight" [booking]
          let flightId = getSecondArg flightFact

          -- Check flight status
          statusFact <- queryPredicate "flight_status" [flightId]
          let status = getSecondArg statusFact

          -- Require status is "Cancelled"
          andAll
            [ require (status == ScStr "Cancelled") "flight is cancelled"
            ],
        -- Flight is delayed
        -- Note: Ideally we'd check flight_delay_minutes > 180, but that
        -- predicate isn't currently available. For now, any delay qualifies.
        withRule "flight_delayed" $ do
          flightFact <- queryPredicate "booking_flight" [booking]
          let flightId = getSecondArg flightFact

          statusFact <- queryPredicate "flight_status" [flightId]
          let status = getSecondArg statusFact

          andAll
            [ require (status == ScStr "Delayed") "flight is delayed"
            ]
      ]

-- | Check if EU261 regulation applies.
--
-- EU261 provides strong passenger rights for flights departing from EU airports
-- when there are significant delays (> 5 hours) or cancellations.
--
-- Note: This is a simplified implementation. The full rule requires:
-- - Flight origin is an EU airport (is_eu_airport predicate not yet implemented)
-- - Delay is > 300 minutes (flight_delay_minutes not yet implemented)
--
-- Currently, this checks if the origin airport is in a known list of EU airports.
eu261Eligible :: Scalar -> SolverM Proof
eu261Eligible booking =
  withRule "eu261_eligible" $ do
    -- Get the flight for this booking
    flightFact <- queryPredicate "booking_flight" [booking]
    let flightId = getSecondArg flightFact

    -- Get flight origin
    originFact <- queryPredicate "flight_origin" [flightId]
    let origin = getSecondArg originFact

    -- Check flight status (must be cancelled or delayed)
    statusFact <- queryPredicate "flight_status" [flightId]
    let status = getSecondArg statusFact

    andAll
      [ -- Origin must be an EU airport
        -- This is a simplified check - in production, this would use an
        -- is_eu_airport predicate backed by reference data
        require (isEuAirport origin) "flight departs from EU airport",
        -- Flight must be cancelled or delayed
        require
          (status == ScStr "Cancelled" || status == ScStr "Delayed")
          "flight is cancelled or delayed"
      ]
  where
    -- Simplified EU airport check
    -- In production, this would be a proper predicate with reference data
    isEuAirport :: Scalar -> Bool
    isEuAirport (ScStr code) =
      code
        `elem` [ "LHR", -- London Heathrow (UK - still covered post-Brexit for EU261)
                 "CDG", -- Paris Charles de Gaulle
                 "FRA", -- Frankfurt
                 "AMS", -- Amsterdam
                 "MAD", -- Madrid
                 "FCO", -- Rome Fiumicino
                 "MUC", -- Munich
                 "BCN", -- Barcelona
                 "ORY", -- Paris Orly
                 "DUB" -- Dublin
               ]
    isEuAirport _ = False

-- | Check if the fare class allows a refund.
--
-- Different fare classes have different refund policies:
-- - Flexible/Business: Full refund allowed
-- - Standard/Economy: Partial refund (with cancellation penalty)
-- - Basic Economy: No refund (must use special exceptions)
fareAllowsRefund :: Scalar -> Text -> RefundType -> SolverM Proof
fareAllowsRefund booking expectedClass expectedType =
  withRule ("fare_allows_" <> refundTypeToText expectedType) $ do
    -- Get fare class for booking
    fareClassFact <- queryPredicate "booking_fare_class" [booking]
    let fareClass = getSecondArg fareClassFact

    -- Check the fare class matches expected
    -- Note: The fare class names from the database are "Business", "Economy", etc.
    -- We map these to policy categories
    let classMatches = case (expectedClass, fareClass) of
          ("flexible", ScStr "Business") -> True
          ("flexible", ScStr "First") -> True
          ("standard", ScStr "Economy") -> True
          ("standard", ScStr "PremiumEconomy") -> True
          _ -> False

    andAll
      [ require classMatches ("fare class is " <> expectedClass)
      ]

-- | Check if bereavement exception applies.
--
-- Bereavement exceptions require:
-- 1. User claims bereavement (askable)
-- 2. User confirms they will provide death certificate (askable)
bereavementEligible :: Scalar -> SolverM Proof
bereavementEligible _booking = do
  -- Get current user from context
  user <- contextVar "current_user"

  withRule "bereavement_eligible" $
    andAll
      [ -- User claims bereavement
        askable "user_claims_bereavement" [user],
        -- User will provide documentation
        askable "user_confirms_documentation" [user, ScStr "death_certificate"]
      ]

-- | Check if medical exception for Basic Economy applies.
--
-- Basic Economy tickets are normally non-refundable, but a medical
-- emergency with documentation qualifies for a travel voucher.
medicalBasicEligible :: Scalar -> SolverM Proof
medicalBasicEligible booking = do
  -- Get current user from context
  user <- contextVar "current_user"

  withRule "medical_basic_eligible" $ do
    -- Check ticket type is Basic Economy
    ticketTypeFact <- queryPredicate "booking_ticket_type" [booking]
    let ticketType = getSecondArg ticketTypeFact

    andAll
      [ -- Must be EconomyBasic ticket
        require (ticketType == ScStr "EconomyBasic") "ticket is Basic Economy",
        -- User claims medical emergency
        askable "user_claims_medical_emergency" [user],
        -- User will provide documentation
        askable "user_confirms_documentation" [user, ScStr "medical_certificate"]
      ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Get the second argument from a BaseFact.
--
-- Most predicates have the pattern @predicate(Key, Value)@ where the first
-- argument is the lookup key and the second is the extracted value.
getSecondArg :: BaseFact -> Scalar
getSecondArg (BaseFact _ args) = case args of
  (_ : v : _) -> v
  _ -> error "getSecondArg: fact has fewer than 2 arguments"
