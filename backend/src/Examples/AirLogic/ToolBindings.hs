-- | Tool bindings for the AirLogic Airlines domain.
--
-- These bindings describe how solver predicates map to AirLogic tools.
-- When the solver needs to prove a predicate, it uses these bindings to
-- invoke the appropriate tool. Facts are produced by the Tool's execute
-- function via ToolOutput.producedFacts.
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
import Sentinel.Solver.ToolBindings
  ( ToolBinding (..),
    ToolBindingRegistry,
    emptyToolBindingRegistry,
    registerBinding,
    singleArgBuilder,
  )

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
      buildArgs = singleArgBuilder "flightId" "flight_status"
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
      buildArgs = singleArgBuilder "flightId" "flight_delay_minutes"
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
      buildArgs = singleArgBuilder "flightId" "flight_departure_airport"
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
      buildArgs = singleArgBuilder "flightId" "flight_arrival_airport"
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
      buildArgs = singleArgBuilder "bookingId" "booking_fare_class"
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
      buildArgs = singleArgBuilder "bookingId" "booking_amount"
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
      buildArgs = singleArgBuilder "bookingId" "booking_flight"
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
      buildArgs = singleArgBuilder "bookingId" "booking_user"
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
      buildArgs = singleArgBuilder "bookingId" "hours_until_departure"
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
      buildArgs = singleArgBuilder "code" "is_eu_airport"
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
      buildArgs = singleArgBuilder "userId" "user_bookings"
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
      buildArgs = singleArgBuilder "userId" "user_loyalty_tier"
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
      buildArgs = singleArgBuilder "fareClass" "rebooking_allowed"
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
      buildArgs = singleArgBuilder "fareClass" "rebooking_fee"
    }
