-- | Domain-specific facts for the Air Canada example.
-- These facts represent knowledge accumulated during conversation.
module Examples.AirCanada.Facts
  ( Fact (..),
    BookingRef,
    FlightNumber,
    UserId,
  )
where

import Data.Time (UTCTime)
import Examples.AirCanada.Refund qualified as Refund
import Examples.AirCanada.Types qualified as Types
import Pre
import Sentinel.Facts (Evidence)

-- | Type aliases for clarity
type BookingRef = Text

type FlightNumber = Text

type UserId = Text

-- | A fact that has been established during the conversation.
-- Fine-grained facts (one per field) enable flexible pattern matching.
data Fact
  = -- Session context (pre-seeded at conversation start)
    LoggedInUser UserId
  | CurrentTime UTCTime
  | -- Booking facts (from RetrieveBooking or SearchBookingsByName)
    BookingExists BookingRef
  | BookingPassenger BookingRef Text
  | BookingFlight BookingRef FlightNumber
  | BookingStatus BookingRef Types.BookingStatus
  | BookingSource BookingRef Refund.BookingSource
  | BookingTicketType BookingRef Refund.TicketType
  | BookingPriceCents BookingRef Int
  | BookingTicketClass BookingRef Text
  | -- Flight facts (from CheckFlightStatus)
    FlightExists FlightNumber
  | FlightStatusFact FlightNumber Types.FlightStatus
  | FlightRoute FlightNumber Text Text -- origin, destination
  | -- User-asserted facts (from questions, with evidence level)
    UserAsserted Fact Evidence
  deriving stock (Eq, Ord, Show, Generic)
