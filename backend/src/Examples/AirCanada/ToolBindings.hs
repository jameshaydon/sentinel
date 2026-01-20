-- | Tool bindings for the Air Canada domain.
--
-- These bindings describe how solver predicates map to Air Canada tools.
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
module Examples.AirCanada.ToolBindings
  ( -- * Registry
    airCanadaToolBindings,

    -- * Individual Bindings
    flightStatusBinding,
    flightOriginBinding,
    flightDestinationBinding,
    bookingFlightBinding,
    bookingFareClassBinding,
    bookingAmountBinding,
    bookingPassengerBinding,
    bookingSourceBinding,
    bookingTicketTypeBinding,
    userBookingsBinding,
  )
where

import Pre
import Sentinel.JSON (extractArrayField, extractNestedString, extractNumber, extractString)
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

-- | Complete tool binding registry for Air Canada domain.
--
-- Contains bindings for all predicates that can be established via tools:
--
-- __Flight predicates__ (from @CheckFlightStatus@):
--
-- - @flight_status(FlightId, Status)@ - OnTime, Delayed, Cancelled, etc.
-- - @flight_origin(FlightId, Airport)@ - departure airport code
-- - @flight_destination(FlightId, Airport)@ - arrival airport code
--
-- __Booking predicates__ (from @RetrieveBooking@):
--
-- - @booking_flight(BookingId, FlightId)@ - associated flight number
-- - @booking_fare_class(BookingId, Class)@ - Economy, Business, etc.
-- - @booking_amount(BookingId, Cents)@ - price in cents
-- - @booking_passenger(BookingId, Name)@ - passenger name
-- - @booking_source(BookingId, Source)@ - DirectAirCanada, TravelAgency, etc.
-- - @booking_ticket_type(BookingId, Type)@ - Refundable, EconomyBasic, etc.
--
-- __User predicates__ (from @SearchBookingsByName@):
--
-- - @user_bookings(UserName, BookingId)@ - bookings for a user
airCanadaToolBindings :: ToolBindingRegistry
airCanadaToolBindings =
  foldl'
    (flip registerBinding)
    emptyToolBindingRegistry
    [ -- Flight predicates
      flightStatusBinding,
      flightOriginBinding,
      flightDestinationBinding,
      -- Booking predicates
      bookingFlightBinding,
      bookingFareClassBinding,
      bookingAmountBinding,
      bookingPassengerBinding,
      bookingSourceBinding,
      bookingTicketTypeBinding,
      -- User predicates
      userBookingsBinding
    ]

--------------------------------------------------------------------------------
-- Flight Bindings
--------------------------------------------------------------------------------

-- | Binding for @flight_status(FlightId, Status)@.
--
-- Extracts the flight status from @CheckFlightStatus@ tool output.
-- Status values: OnTime, Delayed, Cancelled, Boarding, Landed.
flightStatusBinding :: ToolBinding
flightStatusBinding =
  ToolBinding
    { predicate = "flight_status",
      inputArity = 1,
      toolName = "CheckFlightStatus",
      description = "Get flight operational status",
      buildArgs = singleArgBuilder "flightNumber" "flight_status",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            status = extractString "status" result
         in case (flightId, status) of
              (Just fid, Just s) -> [BaseFact "flight_status" [fid, ScStr s]]
              _ -> []
    }

-- | Binding for @flight_origin(FlightId, Airport)@.
--
-- Extracts the departure airport from @CheckFlightStatus@ tool output.
flightOriginBinding :: ToolBinding
flightOriginBinding =
  ToolBinding
    { predicate = "flight_origin",
      inputArity = 1,
      toolName = "CheckFlightStatus",
      description = "Get flight departure airport",
      buildArgs = singleArgBuilder "flightNumber" "flight_origin",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            origin = extractString "origin" result
         in case (flightId, origin) of
              (Just fid, Just o) -> [BaseFact "flight_origin" [fid, ScStr o]]
              _ -> []
    }

-- | Binding for @flight_destination(FlightId, Airport)@.
--
-- Extracts the arrival airport from @CheckFlightStatus@ tool output.
flightDestinationBinding :: ToolBinding
flightDestinationBinding =
  ToolBinding
    { predicate = "flight_destination",
      inputArity = 1,
      toolName = "CheckFlightStatus",
      description = "Get flight arrival airport",
      buildArgs = singleArgBuilder "flightNumber" "flight_destination",
      extractFacts = \inputArgs result ->
        let flightId = headMay inputArgs
            dest = extractString "destination" result
         in case (flightId, dest) of
              (Just fid, Just d) -> [BaseFact "flight_destination" [fid, ScStr d]]
              _ -> []
    }

--------------------------------------------------------------------------------
-- Booking Bindings
--------------------------------------------------------------------------------

-- | Binding for @booking_flight(BookingId, FlightId)@.
--
-- Extracts the associated flight number from @RetrieveBooking@ tool output.
bookingFlightBinding :: ToolBinding
bookingFlightBinding =
  ToolBinding
    { predicate = "booking_flight",
      inputArity = 1,
      toolName = "RetrieveBooking",
      description = "Get flight number for a booking",
      buildArgs = singleArgBuilder "bookingRef" "booking_flight",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            flightNo = extractString "flightNo" result
         in case (bookingId, flightNo) of
              (Just bid, Just fno) -> [BaseFact "booking_flight" [bid, ScStr fno]]
              _ -> []
    }

-- | Binding for @booking_fare_class(BookingId, Class)@.
--
-- Extracts the ticket class from @RetrieveBooking@ tool output.
-- Classes: Economy, Business, First, etc.
bookingFareClassBinding :: ToolBinding
bookingFareClassBinding =
  ToolBinding
    { predicate = "booking_fare_class",
      inputArity = 1,
      toolName = "RetrieveBooking",
      description = "Get ticket class for a booking",
      buildArgs = singleArgBuilder "bookingRef" "booking_fare_class",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            ticketClass = extractString "ticketClass" result
         in case (bookingId, ticketClass) of
              (Just bid, Just tc) -> [BaseFact "booking_fare_class" [bid, ScStr tc]]
              _ -> []
    }

-- | Binding for @booking_amount(BookingId, Cents)@.
--
-- Extracts the price in cents from @RetrieveBooking@ tool output.
bookingAmountBinding :: ToolBinding
bookingAmountBinding =
  ToolBinding
    { predicate = "booking_amount",
      inputArity = 1,
      toolName = "RetrieveBooking",
      description = "Get booking price in cents",
      buildArgs = singleArgBuilder "bookingRef" "booking_amount",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            priceCents = extractNumber "priceCents" result
         in case (bookingId, priceCents) of
              (Just bid, Just cents) -> [BaseFact "booking_amount" [bid, ScNum cents]]
              _ -> []
    }

-- | Binding for @booking_passenger(BookingId, Name)@.
--
-- Extracts the passenger name from @RetrieveBooking@ tool output.
bookingPassengerBinding :: ToolBinding
bookingPassengerBinding =
  ToolBinding
    { predicate = "booking_passenger",
      inputArity = 1,
      toolName = "RetrieveBooking",
      description = "Get passenger name for a booking",
      buildArgs = singleArgBuilder "bookingRef" "booking_passenger",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            passenger = extractString "passengerName" result
         in case (bookingId, passenger) of
              (Just bid, Just name) -> [BaseFact "booking_passenger" [bid, ScStr name]]
              _ -> []
    }

-- | Binding for @booking_source(BookingId, Source)@.
--
-- Extracts the booking source from @RetrieveBooking@ tool output.
-- Sources: DirectAirCanada, TravelAgency, OtherAirline, GroupBooking.
bookingSourceBinding :: ToolBinding
bookingSourceBinding =
  ToolBinding
    { predicate = "booking_source",
      inputArity = 1,
      toolName = "RetrieveBooking",
      description = "Get booking source (direct, agency, etc.)",
      buildArgs = singleArgBuilder "bookingRef" "booking_source",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            -- bookingSource is nested inside ticketDetails
            source = extractNestedString ["ticketDetails", "bookingSource"] result
         in case (bookingId, source) of
              (Just bid, Just s) -> [BaseFact "booking_source" [bid, ScStr s]]
              _ -> []
    }

-- | Binding for @booking_ticket_type(BookingId, Type)@.
--
-- Extracts the ticket type from @RetrieveBooking@ tool output.
-- Types: Refundable, EconomyBasic, OtherNonRefundable.
bookingTicketTypeBinding :: ToolBinding
bookingTicketTypeBinding =
  ToolBinding
    { predicate = "booking_ticket_type",
      inputArity = 1,
      toolName = "RetrieveBooking",
      description = "Get ticket type (refundable, non-refundable, etc.)",
      buildArgs = singleArgBuilder "bookingRef" "booking_ticket_type",
      extractFacts = \inputArgs result ->
        let bookingId = headMay inputArgs
            -- ticketType is nested inside ticketDetails
            ticketType = extractNestedString ["ticketDetails", "ticketType"] result
         in case (bookingId, ticketType) of
              (Just bid, Just tt) -> [BaseFact "booking_ticket_type" [bid, ScStr tt]]
              _ -> []
    }

--------------------------------------------------------------------------------
-- User Bindings
--------------------------------------------------------------------------------

-- | Binding for @user_bookings(UserName, BookingId)@.
--
-- Extracts all booking references from @SearchBookingsByName@ tool output.
-- Returns multiple facts (one per booking found).
userBookingsBinding :: ToolBinding
userBookingsBinding =
  ToolBinding
    { predicate = "user_bookings",
      inputArity = 1,
      toolName = "SearchBookingsByName",
      description = "Get all bookings for a user",
      buildArgs = singleArgBuilder "passengerName" "user_bookings",
      extractFacts = \inputArgs result ->
        let userName = headMay inputArgs
            -- Result is an array of bookings; extract bookingRef from each
            bookingRefs = extractArrayField "bookingRef" result
         in case userName of
              Just uid ->
                [BaseFact "user_bookings" [uid, ScStr ref] | ref <- bookingRefs]
              Nothing -> []
    }
