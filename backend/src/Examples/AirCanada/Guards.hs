-- | Guard implementations for Air Canada tools.
--
-- Guards are preconditions that must be satisfied before a tool can execute.
-- They query the fact database to verify required conditions.
module Examples.AirCanada.Guards
  ( userIdentityGuard,
    searchBookingsGuard,
    refundGuard,
  )
where

import Pre
import Sentinel.Solver.Combinators (extractArg, oneOf, queryPredicate)
import Sentinel.Solver.Types (Proof (..), Scalar (..))
import Sentinel.Tool (Guard)

-- | Guard that requires user identity to be established.
--
-- Queries the "logged_in_user" predicate to verify a user is logged in.
userIdentityGuard :: Guard
userIdentityGuard _args = do
  fact <- queryPredicate "logged_in_user" []
  pure $ FactUsed fact

-- | Guard for SearchBookingsByName - requires identity AND name must match logged-in user.
--
-- This guard:
-- 1. Requires a user to be logged in
-- 2. Verifies the passengerName argument matches the logged-in user
searchBookingsGuard :: Guard
searchBookingsGuard args = do
  name <- extractArg "passengerName" args
  fact <- queryPredicate "logged_in_user" [name]
  pure $ FactUsed fact

-- | Guard for refund tool.
--
-- Requires:
-- - User identity established
-- - Booking must exist (will be auto-fetched via tool binding)
-- - Booking source must not be TravelAgency or OtherAirline
refundGuard :: Guard
refundGuard args = do
  bookingRef <- extractArg "bookingRef" args
  -- Require user identity
  _ <- queryPredicate "logged_in_user" []
  -- Booking must exist (auto-fetched via tool binding)
  _ <- queryPredicate "booking_passenger" [bookingRef]
  -- Booking source must be acceptable (not from travel agency or other airline)
  oneOf
    [ do
        fact <- queryPredicate "booking_source" [bookingRef, ScStr "DirectAirCanada"]
        pure $ FactUsed fact,
      do
        fact <- queryPredicate "booking_source" [bookingRef, ScStr "GroupBooking"]
        pure $ FactUsed fact
    ]
