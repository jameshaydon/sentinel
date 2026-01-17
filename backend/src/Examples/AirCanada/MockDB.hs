module Examples.AirCanada.MockDB
  ( initialDB,
    getBooking,
    getFlight,
    getFlightStatus,
    attemptRefund,
    listBookingsForPassenger,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time
import Examples.AirCanada.Types
import Pre

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

    -- Sample bookings
    booking1 =
      Booking
        { bookingRef = "REF123",
          passengerName = "Alice Smith",
          flightNo = "AC101",
          ticketClass = "Business",
          isRefundable = True,
          priceCents = 250000
        }

    booking2 =
      Booking
        { bookingRef = "REF456",
          passengerName = "Bob Jones",
          flightNo = "AC102",
          ticketClass = "Economy",
          isRefundable = False,
          priceCents = 45000
        }

    booking3 =
      Booking
        { bookingRef = "REF789",
          passengerName = "Carol White",
          flightNo = "AC103",
          ticketClass = "Economy",
          isRefundable = True,
          priceCents = 85000
        }

    booking4 =
      Booking
        { bookingRef = "REF111",
          passengerName = "Alice Smith",
          flightNo = "AC104",
          ticketClass = "Economy",
          isRefundable = False,
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

-- | Attempt to process a refund for a booking.
attemptRefund :: Text -> AirlineDB -> Text
attemptRefund ref db =
  case getBooking ref db of
    Nothing -> "Error: Booking reference '" <> ref <> "' not found."
    Just booking ->
      if booking.isRefundable
        then
          "Success: Refund of "
            <> formatCents booking.priceCents
            <> " processed for booking "
            <> booking.bookingRef
            <> ". Amount will be credited to the original payment method within 5-7 business days."
        else
          "Failure: Booking "
            <> booking.bookingRef
            <> " is marked as Non-Refundable. "
            <> "However, since flight "
            <> booking.flightNo
            <> " may be affected by operational issues, "
            <> "you may be eligible for a travel credit or rebooking. "
            <> "Please contact customer service for assistance."
