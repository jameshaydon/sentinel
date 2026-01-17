module Examples.AirCanada.Types
  ( FlightStatus (..),
    Flight (..),
    Booking (..),
    AirlineDB (..),
    formatCents,
  )
where

import Data.Text qualified as T
import Data.Time (UTCTime)
import Pre

-- | Format cents as dollars with proper zero-padding.
formatCents :: Int -> Text
formatCents cents =
  let dollars = cents `div` 100
      remainder = cents `mod` 100
   in "$" <> T.pack (show dollars) <> "." <> T.justifyRight 2 '0' (T.pack (show remainder))

-- | Represents the operational status of a specific flight segment.
data FlightStatus
  = OnTime
  | Delayed
  | Cancelled
  | Boarding
  | Landed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp FlightStatus where
  disp = \case
    OnTime -> "OnTime"
    Delayed -> "Delayed"
    Cancelled -> "Cancelled"
    Boarding -> "Boarding"
    Landed -> "Landed"

-- | Represents a scheduled flight segment.
data Flight = Flight
  { flightNumber :: Text,
    origin :: Text,
    destination :: Text,
    scheduledDeparture :: UTCTime,
    scheduledArrival :: UTCTime,
    status :: FlightStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Flight where
  disp f =
    vsep
      [ "Flight:" <+> pretty f.flightNumber,
        "Route:" <+> pretty f.origin <+> "→" <+> pretty f.destination,
        "Departure:" <+> pretty (show f.scheduledDeparture),
        "Arrival:" <+> pretty (show f.scheduledArrival),
        "Status:" <+> disp f.status
      ]

-- | Represents a passenger's reservation.
data Booking = Booking
  { bookingRef :: Text,
    passengerName :: Text,
    flightNo :: Text,
    ticketClass :: Text,
    isRefundable :: Bool,
    priceCents :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Booking where
  disp b =
    vsep
      [ "Booking Reference:" <+> pretty b.bookingRef,
        "Passenger:" <+> pretty b.passengerName,
        "Flight:" <+> pretty b.flightNo,
        "Class:" <+> pretty b.ticketClass,
        "Price:" <+> pretty (formatCents b.priceCents),
        "Refundable:" <+> if b.isRefundable then "Yes" else "No"
      ]

-- | The complete state of our mock airline system.
data AirlineDB = AirlineDB
  { flightsTable :: Map Text Flight,
    bookingsTable :: Map Text Booking
  }
  deriving stock (Show)
