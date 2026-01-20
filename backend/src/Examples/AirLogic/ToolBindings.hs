-- | Tool bindings for the AirLogic Airlines domain.
--
-- These bindings describe how solver predicates map to AirLogic tools.
-- When the solver needs to prove a predicate, it uses these bindings to
-- invoke the appropriate tool and extract facts from the result.
--
-- === Predicate Naming Convention
--
-- Predicates follow a consistent naming pattern:
--
-- - @entity_property(EntityId, Value)@ - e.g., @flight_status(FlightId, Status)@
-- - First argument is typically the lookup key (input)
-- - Subsequent arguments are values extracted from tool output (outputs)
module Examples.AirLogic.ToolBindings
  ( -- * Registry
    airLogicToolBindings,

    -- * Flight Bindings
    flightStatusBinding,
    flightDelayMinutesBinding,
    flightDepartureAirportBinding,
    flightArrivalAirportBinding,

    -- * Booking Bindings
    bookingFareClassBinding,
    bookingAmountBinding,
    bookingFlightBinding,
    bookingUserBinding,
    hoursUntilDepartureBinding,

    -- * Airport Bindings
    isEUAirportBinding,

    -- * User Bindings
    userBookingsBinding,
    userLoyaltyTierBinding,

    -- * Rebooking Bindings
    rebookingAllowedBinding,
    rebookingFeeBinding,
  )
where

import Pre
import Sentinel.JSON (extractArrayField, extractBool, extractNumber, extractString)
import Sentinel.Solver.ToolBindings
  ( ToolBinding (..),
    ToolBindingRegistry,
    emptyToolBindingRegistry,
    registerBinding,
    singleArgBuilder,
  )
import Sentinel.Solver.Types (BaseFact (..), Scalar (..))

--------------------------------------------------------------------------------
-- Registry
--------------------------------------------------------------------------------

-- | Complete tool binding registry for AirLogic Airlines domain.
--
-- Contains bindings for all predicates that can be established via tools:
--
-- __Flight predicates__ (from @GetFlightDetails@):
--
-- - @flight_status(FlightId, Status)@ - OnTime, Delayed, Cancelled
-- - @flight_delay_minutes(FlightId, Minutes)@ - delay in minutes
-- - @flight_departure_airport(FlightId, Airport)@ - departure airport code
-- - @flight_arrival_airport(FlightId, Airport)@ - arrival airport code
--
-- __Booking predicates__ (from @GetBooking@):
--
-- - @booking_fare_class(BookingId, FareClass)@ - Flexible, Standard, Basic
-- - @booking_amount(BookingId, Cents)@ - price in cents
-- - @booking_flight(BookingId, FlightId)@ - associated flight
-- - @booking_user(BookingId, UserId)@ - associated user
-- - @hours_until_departure(BookingId, Hours)@ - hours until flight
--
-- __Airport predicates__ (from @GetAirportInfo@):
--
-- - @is_eu_airport(AirportCode)@ - boolean predicate for EU airports
--
-- __User predicates__ (from @GetUserBookings@, @GetUserProfile@):
--
-- - @user_bookings(UserId, BookingId)@ - bookings for a user
-- - @user_loyalty_tier(UserId, Tier)@ - loyalty tier
--
-- __Rebooking predicates__ (from @GetRebookingPolicy@):
--
-- - @rebooking_allowed(FareClass)@ - boolean predicate
-- - @rebooking_fee(FareClass, Fee)@ - change fee in cents
airLogicToolBindings :: ToolBindingRegistry
airLogicToolBindings =
  foldl'
    (flip registerBinding)
    emptyToolBindingRegistry
    [ -- Flight predicates
      flightStatusBinding,
      flightDelayMinutesBinding,
      flightDepartureAirportBinding,
      flightArrivalAirportBinding,
      -- Booking predicates
      bookingFareClassBinding,
      bookingAmountBinding,
      bookingFlightBinding,
      bookingUserBinding,
      hoursUntilDepartureBinding,
      -- Airport predicates
      isEUAirportBinding,
      -- User predicates
      userBookingsBinding,
      userLoyaltyTierBinding,
      -- Rebooking predicates
      rebookingAllowedBinding,
      rebookingFeeBinding
    ]

--------------------------------------------------------------------------------
-- Flight Bindings
--------------------------------------------------------------------------------

-- | Binding for @flight_status(FlightId, Status)@.
--
-- Extracts the flight status from @GetFlightDetails@ tool output.
-- Status values: OnTime, Delayed, Cancelled.
flightStatusBinding :: ToolBinding
flightStatusBinding =
  ToolBinding
    { predicate = "flight_status",
      inputArity = 1,
      toolName = "GetFlightDetails",
      description = "Get flight operational status",
      buildArgs = singleArgBuilder "flightId" "flight_status",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            status = extractString "status" result
         in case (flightId, status) of
              (Just fid, Just s) -> [BaseFact "flight_status" [fid, ScStr s]]
              _ -> []
    }

-- | Binding for @flight_delay_minutes(FlightId, Minutes)@.
--
-- Extracts the delay in minutes from @GetFlightDetails@ tool output.
flightDelayMinutesBinding :: ToolBinding
flightDelayMinutesBinding =
  ToolBinding
    { predicate = "flight_delay_minutes",
      inputArity = 1,
      toolName = "GetFlightDetails",
      description = "Get flight delay in minutes",
      buildArgs = singleArgBuilder "flightId" "flight_delay_minutes",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            delay = extractNumber "delayMinutes" result
         in case (flightId, delay) of
              (Just fid, Just d) -> [BaseFact "flight_delay_minutes" [fid, ScNum d]]
              _ -> []
    }

-- | Binding for @flight_departure_airport(FlightId, Airport)@.
--
-- Extracts the departure airport from @GetFlightDetails@ tool output.
flightDepartureAirportBinding :: ToolBinding
flightDepartureAirportBinding =
  ToolBinding
    { predicate = "flight_departure_airport",
      inputArity = 1,
      toolName = "GetFlightDetails",
      description = "Get flight departure airport",
      buildArgs = singleArgBuilder "flightId" "flight_departure_airport",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            airport = extractString "departureAirport" result
         in case (flightId, airport) of
              (Just fid, Just a) -> [BaseFact "flight_departure_airport" [fid, ScStr a]]
              _ -> []
    }

-- | Binding for @flight_arrival_airport(FlightId, Airport)@.
--
-- Extracts the arrival airport from @GetFlightDetails@ tool output.
flightArrivalAirportBinding :: ToolBinding
flightArrivalAirportBinding =
  ToolBinding
    { predicate = "flight_arrival_airport",
      inputArity = 1,
      toolName = "GetFlightDetails",
      description = "Get flight arrival airport",
      buildArgs = singleArgBuilder "flightId" "flight_arrival_airport",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            airport = extractString "arrivalAirport" result
         in case (flightId, airport) of
              (Just fid, Just a) -> [BaseFact "flight_arrival_airport" [fid, ScStr a]]
              _ -> []
    }

--------------------------------------------------------------------------------
-- Booking Bindings
--------------------------------------------------------------------------------

-- | Binding for @booking_fare_class(BookingId, FareClass)@.
--
-- Extracts the fare class from @GetBooking@ tool output.
-- Fare classes: Flexible, Standard, Basic.
bookingFareClassBinding :: ToolBinding
bookingFareClassBinding =
  ToolBinding
    { predicate = "booking_fare_class",
      inputArity = 1,
      toolName = "GetBooking",
      description = "Get booking fare class",
      buildArgs = singleArgBuilder "bookingId" "booking_fare_class",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            fareClass = extractString "fareClass" result
         in case (bookingId, fareClass) of
              (Just bid, Just fc) -> [BaseFact "booking_fare_class" [bid, ScStr fc]]
              _ -> []
    }

-- | Binding for @booking_amount(BookingId, Cents)@.
--
-- Extracts the booking amount in cents from @GetBooking@ tool output.
bookingAmountBinding :: ToolBinding
bookingAmountBinding =
  ToolBinding
    { predicate = "booking_amount",
      inputArity = 1,
      toolName = "GetBooking",
      description = "Get booking amount in cents",
      buildArgs = singleArgBuilder "bookingId" "booking_amount",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            amount = extractNumber "totalAmountCents" result
         in case (bookingId, amount) of
              (Just bid, Just a) -> [BaseFact "booking_amount" [bid, ScNum a]]
              _ -> []
    }

-- | Binding for @booking_flight(BookingId, FlightId)@.
--
-- Extracts the associated flight from @GetBooking@ tool output.
bookingFlightBinding :: ToolBinding
bookingFlightBinding =
  ToolBinding
    { predicate = "booking_flight",
      inputArity = 1,
      toolName = "GetBooking",
      description = "Get booking's flight ID",
      buildArgs = singleArgBuilder "bookingId" "booking_flight",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            flightId = extractString "flightId" result
         in case (bookingId, flightId) of
              (Just bid, Just fid) -> [BaseFact "booking_flight" [bid, ScStr fid]]
              _ -> []
    }

-- | Binding for @booking_user(BookingId, UserId)@.
--
-- Extracts the associated user from @GetBooking@ tool output.
bookingUserBinding :: ToolBinding
bookingUserBinding =
  ToolBinding
    { predicate = "booking_user",
      inputArity = 1,
      toolName = "GetBooking",
      description = "Get booking's user ID",
      buildArgs = singleArgBuilder "bookingId" "booking_user",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            userId = extractString "userId" result
         in case (bookingId, userId) of
              (Just bid, Just uid) -> [BaseFact "booking_user" [bid, ScStr uid]]
              _ -> []
    }

-- | Binding for @hours_until_departure(BookingId, Hours)@.
--
-- Extracts hours until departure from @GetBooking@ tool output.
hoursUntilDepartureBinding :: ToolBinding
hoursUntilDepartureBinding =
  ToolBinding
    { predicate = "hours_until_departure",
      inputArity = 1,
      toolName = "GetBooking",
      description = "Get hours until booking's flight departure",
      buildArgs = singleArgBuilder "bookingId" "hours_until_departure",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            hours = extractNumber "hoursUntilDeparture" result
         in case (bookingId, hours) of
              (Just bid, Just h) -> [BaseFact "hours_until_departure" [bid, ScNum h]]
              _ -> []
    }

--------------------------------------------------------------------------------
-- Airport Bindings
--------------------------------------------------------------------------------

-- | Binding for @is_eu_airport(AirportCode)@.
--
-- This is a boolean predicate - a fact is produced only if the airport is in EU.
isEUAirportBinding :: ToolBinding
isEUAirportBinding =
  ToolBinding
    { predicate = "is_eu_airport",
      inputArity = 1,
      toolName = "GetAirportInfo",
      description = "Check if airport is in EU (for EU261 eligibility)",
      buildArgs = singleArgBuilder "code" "is_eu_airport",
      extractFacts = \inputArgs result ->
        let code = headMay inputArgs
            isEU = extractBool "isEU" result
         in case (code, isEU) of
              (Just c, Just True) -> [BaseFact "is_eu_airport" [c]]
              _ -> [] -- No fact if not EU or missing data
    }

--------------------------------------------------------------------------------
-- User Bindings
--------------------------------------------------------------------------------

-- | Binding for @user_bookings(UserId, BookingId)@.
--
-- Extracts all booking IDs from @GetUserBookings@ tool output.
-- Returns multiple facts (one per booking found).
userBookingsBinding :: ToolBinding
userBookingsBinding =
  ToolBinding
    { predicate = "user_bookings",
      inputArity = 1,
      toolName = "GetUserBookings",
      description = "Get all bookings for a user",
      buildArgs = singleArgBuilder "userId" "user_bookings",
      extractFacts = \inputArgs result ->
        let userId = headMay inputArgs
            -- Result is an array of bookings; extract bookingId from each
            bookingIds = extractArrayField "bookingId" result
         in case userId of
              Just uid ->
                [BaseFact "user_bookings" [uid, ScStr bid] | bid <- bookingIds]
              Nothing -> []
    }

-- | Binding for @user_loyalty_tier(UserId, Tier)@.
--
-- Extracts the loyalty tier from @GetUserProfile@ tool output.
userLoyaltyTierBinding :: ToolBinding
userLoyaltyTierBinding =
  ToolBinding
    { predicate = "user_loyalty_tier",
      inputArity = 1,
      toolName = "GetUserProfile",
      description = "Get user's loyalty tier",
      buildArgs = singleArgBuilder "userId" "user_loyalty_tier",
      extractFacts = \inputArgs result ->
        let userId = headMay inputArgs
            tier = extractString "loyaltyTier" result
         in case (userId, tier) of
              (Just uid, Just t) -> [BaseFact "user_loyalty_tier" [uid, ScStr t]]
              _ -> []
    }

--------------------------------------------------------------------------------
-- Rebooking Bindings
--------------------------------------------------------------------------------

-- | Binding for @rebooking_allowed(FareClass)@.
--
-- This is a boolean predicate - a fact is produced only if rebooking is allowed.
rebookingAllowedBinding :: ToolBinding
rebookingAllowedBinding =
  ToolBinding
    { predicate = "rebooking_allowed",
      inputArity = 1,
      toolName = "GetRebookingPolicy",
      description = "Check if rebooking is allowed for fare class",
      buildArgs = singleArgBuilder "fareClass" "rebooking_allowed",
      extractFacts = \inputArgs result ->
        let fareClass = headMay inputArgs
            allowed = extractBool "rebookingAllowed" result
         in case (fareClass, allowed) of
              (Just fc, Just True) -> [BaseFact "rebooking_allowed" [fc]]
              _ -> [] -- No fact if not allowed or missing data
    }

-- | Binding for @rebooking_fee(FareClass, Fee)@.
--
-- Extracts the rebooking fee from @GetRebookingPolicy@ tool output.
rebookingFeeBinding :: ToolBinding
rebookingFeeBinding =
  ToolBinding
    { predicate = "rebooking_fee",
      inputArity = 1,
      toolName = "GetRebookingPolicy",
      description = "Get rebooking change fee for fare class",
      buildArgs = singleArgBuilder "fareClass" "rebooking_fee",
      extractFacts = \inputArgs result ->
        let fareClass = headMay inputArgs
            fee = extractNumber "changeFee" result
         in case (fareClass, fee) of
              (Just fc, Just f) -> [BaseFact "rebooking_fee" [fc, ScNum f]]
              _ -> []
    }
