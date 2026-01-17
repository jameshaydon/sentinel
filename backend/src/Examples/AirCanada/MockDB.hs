module Examples.AirCanada.MockDB
  ( initialDB,
    getBooking,
    getFlight,
    getFlightStatus,
    attemptRefund,
    listBookingsForPassenger,
    currentTime,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time
import Examples.AirCanada.Refund
import Examples.AirCanada.Types
import Pre

-- | Fixed "current time" for deterministic mock behavior.
-- This is set to January 19, 2025 at 10:00 UTC (day before flights).
currentTime :: UTCTime
currentTime = UTCTime (fromGregorian 2025 1 19) (10 * 3600)

-- | Initialize the mock database with sample data.
initialDB :: AirlineDB
initialDB =
  AirlineDB
    { flightsTable =
        M.fromList
          [ ("AC101", flight1),
            ("AC102", flight2),
            ("AC103", flight3),
            ("AC104", flight4)
          ],
      bookingsTable =
        M.fromList
          [ ("REF123", booking1),
            ("REF456", booking2),
            ("REF789", booking3),
            ("REF111", booking4)
          ]
    }
  where
    -- Sample flights
    baseTime = UTCTime (fromGregorian 2025 1 20) (8 * 3600)

    flight1 =
      Flight
        { flightNumber = "AC101",
          origin = "YYZ",
          destination = "LHR",
          scheduledDeparture = baseTime,
          scheduledArrival = addUTCTime (7 * 3600) baseTime,
          status = OnTime
        }

    flight2 =
      Flight
        { flightNumber = "AC102",
          origin = "YVR",
          destination = "SFO",
          scheduledDeparture = addUTCTime (2 * 3600) baseTime,
          scheduledArrival = addUTCTime (4 * 3600) baseTime,
          status = Delayed
        }

    flight3 =
      Flight
        { flightNumber = "AC103",
          origin = "YUL",
          destination = "CDG",
          scheduledDeparture = addUTCTime (4 * 3600) baseTime,
          scheduledArrival = addUTCTime (11 * 3600) baseTime,
          status = Cancelled
        }

    flight4 =
      Flight
        { flightNumber = "AC104",
          origin = "YYZ",
          destination = "JFK",
          scheduledDeparture = addUTCTime (6 * 3600) baseTime,
          scheduledArrival = addUTCTime (8 * 3600) baseTime,
          status = Boarding
        }

    -- Purchase time: 3 days before baseTime
    purchaseTime = addUTCTime (-3 * 24 * 3600) baseTime

    -- Sample bookings with rich ticket details

    -- REF123: Refundable Business, flight OnTime → voluntary refund with penalty
    booking1 =
      Booking
        { bookingRef = "REF123",
          passengerName = "Alice Smith",
          flightNo = "AC101",
          ticketClass = "Business",
          ticketDetails =
            TicketDetails
              { ticketType = Refundable,
                ticketFormat = Electronic,
                purchaseTime = purchaseTime,
                bookingSource = DirectAirCanada,
                usage = Unused,
                cancellationPenalty = Money 25000
              },
          priceCents = 250000
        }

    -- REF456: EconomyBasic, flight Delayed → involuntary (full refund)
    booking2 =
      Booking
        { bookingRef = "REF456",
          passengerName = "Bob Jones",
          flightNo = "AC102",
          ticketClass = "Economy",
          ticketDetails =
            TicketDetails
              { ticketType = EconomyBasic,
                ticketFormat = Electronic,
                purchaseTime = purchaseTime,
                bookingSource = DirectAirCanada,
                usage = Unused,
                cancellationPenalty = Money 0
              },
          priceCents = 45000
        }

    -- REF789: OtherNonRefundable, flight Cancelled → full involuntary refund
    booking3 =
      Booking
        { bookingRef = "REF789",
          passengerName = "Carol White",
          flightNo = "AC103",
          ticketClass = "Economy",
          ticketDetails =
            TicketDetails
              { ticketType = OtherNonRefundable,
                ticketFormat = Electronic,
                purchaseTime = purchaseTime,
                bookingSource = DirectAirCanada,
                usage = Unused,
                cancellationPenalty = Money 0
              },
          priceCents = 85000
        }

    -- REF111: TravelAgency booking → must contact agency
    booking4 =
      Booking
        { bookingRef = "REF111",
          passengerName = "Alice Smith",
          flightNo = "AC104",
          ticketClass = "Economy",
          ticketDetails =
            TicketDetails
              { ticketType = OtherNonRefundable,
                ticketFormat = Electronic,
                purchaseTime = purchaseTime,
                bookingSource = TravelAgency,
                usage = Unused,
                cancellationPenalty = Money 0
              },
          priceCents = 35000
        }

-- | Retrieve a booking by reference.
getBooking :: Text -> AirlineDB -> Maybe Booking
getBooking ref db = M.lookup (T.toUpper $ T.strip ref) db.bookingsTable

-- | Retrieve a flight by number.
getFlight :: Text -> AirlineDB -> Maybe Flight
getFlight fNum db = M.lookup (T.toUpper $ T.strip fNum) db.flightsTable

-- | Get the status of a flight.
getFlightStatus :: Text -> AirlineDB -> Maybe FlightStatus
getFlightStatus fNum db = (.status) <$> getFlight fNum db

-- | List all bookings for a passenger name.
listBookingsForPassenger :: Text -> AirlineDB -> [Booking]
listBookingsForPassenger name db =
  filter matchesName $ M.elems db.bookingsTable
  where
    normalizedName = T.toLower $ T.strip name
    matchesName b = T.toLower b.passengerName == normalizedName

-- | Infer refund reason from flight status.
inferRefundReason :: FlightStatus -> Maybe SpecialException -> RefundReason
inferRefundReason flightStatus specialReason =
  case specialReason of
    Just exc -> SpecialCase exc
    Nothing -> case flightStatus of
      Cancelled -> Involuntary FlightCancelled
      Delayed -> Involuntary FlightDelayed
      _ -> Voluntary

-- | Build a Ticket from booking data.
bookingToTicket :: Booking -> Ticket
bookingToTicket b =
  Ticket
    { ticketType = b.ticketDetails.ticketType,
      ticketFormat = b.ticketDetails.ticketFormat,
      pricePaid = Money b.priceCents,
      purchaseTime = b.ticketDetails.purchaseTime,
      bookingSource = b.ticketDetails.bookingSource,
      usage = b.ticketDetails.usage,
      cancellationPenalty = b.ticketDetails.cancellationPenalty
    }

-- | Format a RefundOutcome as user-friendly text.
formatOutcome :: Text -> RefundOutcome -> Text
formatOutcome bookingRef outcome = case outcome of
  FullRefund (Money amt) ->
    "Success: Full refund of "
      <> formatCents amt
      <> " approved for booking "
      <> bookingRef
      <> ". Amount will be credited to the original payment method within 5-7 business days."
  PartialRefund (Money amt) ->
    "Success: Partial refund of "
      <> formatCents amt
      <> " approved for booking "
      <> bookingRef
      <> " (cancellation penalty applied). "
      <> "Amount will be credited to the original payment method within 5-7 business days."
  TravelCredit (Money amt) ->
    "Your booking "
      <> bookingRef
      <> " is eligible for a travel credit of "
      <> formatCents amt
      <> " valid for 12 months. "
      <> "This credit can be used for future Air Canada bookings."
  ACWalletFunds (Money amt) ->
    "Your booking "
      <> bookingRef
      <> " qualifies for "
      <> formatCents amt
      <> " in AC Wallet funds."
  NoRefund ->
    "Unfortunately, booking "
      <> bookingRef
      <> " is not eligible for a refund. "
      <> "Economy Basic tickets are non-refundable and do not qualify for travel credits."
  ContactAgency src ->
    "Booking "
      <> bookingRef
      <> " was purchased through a "
      <> (case src of TravelAgency -> "travel agency"; OtherAirline -> "partner airline"; _ -> "third party")
      <> ". Please contact them directly to process your refund request."
  RefundPlusFreeReturn (Money amt) ->
    "Success: Full refund of "
      <> formatCents amt
      <> " approved for booking "
      <> bookingRef
      <> ", plus a free return flight if you are currently away from home. "
      <> "Amount will be credited within 5-7 business days."

-- | Attempt to process a refund for a booking.
-- Accepts an optional special reason (jury, military, death).
attemptRefund :: Text -> Maybe SpecialException -> AirlineDB -> Text
attemptRefund ref specialReason db =
  case getBooking ref db of
    Nothing -> "Error: Booking reference '" <> ref <> "' not found."
    Just booking ->
      case getFlight booking.flightNo db of
        Nothing -> "Error: Flight " <> booking.flightNo <> " not found in system."
        Just flight ->
          let reason = inferRefundReason flight.status specialReason
              ticket = bookingToTicket booking
              request =
                RefundRequest
                  { ticket = ticket,
                    reason = reason,
                    requestTime = currentTime,
                    tripNowPointless = flight.status == Cancelled,
                    hasDocumentation = isJust specialReason -- assume docs provided if special reason given
                  }
              outcome = calculateRefund request
           in formatOutcome booking.bookingRef outcome
