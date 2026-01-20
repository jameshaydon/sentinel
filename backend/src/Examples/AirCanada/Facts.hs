-- | Fact production functions for Air Canada tools.
--
-- These functions convert domain objects (Booking, Flight) into BaseFacts
-- that can be queried by the Sentinel solver.
module Examples.AirCanada.Facts
  ( bookingToFacts,
    flightToFacts,
  )
where

import Data.Text qualified as T
import Examples.AirCanada.Types
import Pre
import Sentinel.Solver.Types (BaseFact (..), Scalar (..))

-- | Convert a booking to its constituent BaseFacts.
--
-- Produces facts matching the predicates defined in ToolBindings:
-- - booking_passenger(BookingRef, PassengerName)
-- - booking_flight(BookingRef, FlightNo)
-- - booking_status(BookingRef, Status)
-- - booking_source(BookingRef, Source)
-- - booking_ticket_type(BookingRef, TicketType)
-- - booking_amount(BookingRef, PriceCents)
-- - booking_fare_class(BookingRef, TicketClass)
bookingToFacts :: Booking -> [BaseFact]
bookingToFacts b =
  [ BaseFact "booking_passenger" [ScStr b.bookingRef, ScStr b.passengerName],
    BaseFact "booking_flight" [ScStr b.bookingRef, ScStr b.flightNo],
    BaseFact "booking_status" [ScStr b.bookingRef, ScStr (T.pack $ show b.bookingStatus)],
    BaseFact "booking_source" [ScStr b.bookingRef, ScStr (T.pack $ show b.ticketDetails.bookingSource)],
    BaseFact "booking_ticket_type" [ScStr b.bookingRef, ScStr (T.pack $ show b.ticketDetails.ticketType)],
    BaseFact "booking_amount" [ScStr b.bookingRef, ScNum (fromIntegral b.priceCents)],
    BaseFact "booking_fare_class" [ScStr b.bookingRef, ScStr b.ticketClass]
  ]

-- | Convert a flight to its constituent BaseFacts.
--
-- Produces facts matching the predicates defined in ToolBindings:
-- - flight_status(FlightNumber, Status)
-- - flight_origin(FlightNumber, Origin)
-- - flight_destination(FlightNumber, Destination)
flightToFacts :: Flight -> [BaseFact]
flightToFacts f =
  [ BaseFact "flight_status" [ScStr f.flightNumber, ScStr (T.pack $ show f.status)],
    BaseFact "flight_origin" [ScStr f.flightNumber, ScStr f.origin],
    BaseFact "flight_destination" [ScStr f.flightNumber, ScStr f.destination]
  ]
