-- | Domain types for the AirLogic Airlines example.
--
-- This module defines the core data types for the AirLogic refund policy example,
-- including flights, bookings, airports, users, and rebooking policies.
module Examples.AirLogic.Types
  ( -- * Flight Types
    FlightStatus (..),
    Flight (..),

    -- * Fare Types
    FareClass (..),

    -- * Booking Types
    Booking (..),

    -- * Airport Types
    Airport (..),

    -- * User Types
    User (..),
    LoyaltyTier (..),

    -- * Rebooking Types
    RebookingPolicy (..),

    -- * Database Type
    AirLogicDB (..),

    -- * Utilities
    formatCents,
  )
where

import Data.Text qualified as T
import Data.Time (UTCTime)
import Pre

--------------------------------------------------------------------------------
-- Flight Types
--------------------------------------------------------------------------------

-- | Represents the operational status of a flight.
data FlightStatus
  = OnTime
  | Delayed
  | Cancelled
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp FlightStatus where
  disp = \case
    OnTime -> "On Time"
    Delayed -> "Delayed"
    Cancelled -> "Cancelled"

-- | Represents a scheduled flight.
data Flight = Flight
  { flightId :: Text,
    departureAirport :: Text,
    arrivalAirport :: Text,
    status :: FlightStatus,
    delayMinutes :: Int,
    departureTime :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Flight where
  disp f =
    vsep
      [ "Flight:" <+> pretty f.flightId,
        "Route:" <+> pretty f.departureAirport <+> "→" <+> pretty f.arrivalAirport,
        "Status:" <+> disp f.status,
        "Delay:" <+> pretty f.delayMinutes <+> "minutes",
        "Departure:" <+> pretty (show f.departureTime)
      ]

--------------------------------------------------------------------------------
-- Fare Types
--------------------------------------------------------------------------------

-- | Represents the fare class of a booking.
data FareClass
  = Flexible
  | Standard
  | Basic
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp FareClass where
  disp = \case
    Flexible -> "Flexible"
    Standard -> "Standard"
    Basic -> "Basic"

--------------------------------------------------------------------------------
-- Booking Types
--------------------------------------------------------------------------------

-- | Represents a passenger's booking.
data Booking = Booking
  { bookingId :: Text,
    userId :: Text,
    flightId :: Text,
    fareClass :: FareClass,
    totalAmountCents :: Int,
    hoursUntilDeparture :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Booking where
  disp b =
    vsep
      [ "Booking:" <+> pretty b.bookingId,
        "User:" <+> pretty b.userId,
        "Flight:" <+> pretty b.flightId,
        "Fare Class:" <+> disp b.fareClass,
        "Amount:" <+> pretty (formatCents b.totalAmountCents),
        "Hours until departure:" <+> pretty b.hoursUntilDeparture
      ]

--------------------------------------------------------------------------------
-- Airport Types
--------------------------------------------------------------------------------

-- | Represents an airport.
data Airport = Airport
  { code :: Text,
    name :: Text,
    isEU :: Bool,
    country :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Airport where
  disp a =
    vsep
      [ "Airport:" <+> pretty a.code,
        "Name:" <+> pretty a.name,
        "Country:" <+> pretty a.country,
        "EU Airport:" <+> (if a.isEU then "Yes" else "No")
      ]

--------------------------------------------------------------------------------
-- User Types
--------------------------------------------------------------------------------

-- | Represents a user's loyalty tier.
data LoyaltyTier
  = Bronze
  | Silver
  | Gold
  | Platinum
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp LoyaltyTier where
  disp = \case
    Bronze -> "Bronze"
    Silver -> "Silver"
    Gold -> "Gold"
    Platinum -> "Platinum"

-- | Represents a user profile.
data User = User
  { oduserId :: Text,
    name :: Text,
    email :: Text,
    loyaltyTier :: LoyaltyTier
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp User where
  disp u =
    vsep
      [ "User:" <+> pretty u.oduserId,
        "Name:" <+> pretty u.name,
        "Email:" <+> pretty u.email,
        "Loyalty Tier:" <+> disp u.loyaltyTier
      ]

--------------------------------------------------------------------------------
-- Rebooking Types
--------------------------------------------------------------------------------

-- | Represents a rebooking policy for a fare class.
data RebookingPolicy = RebookingPolicy
  { policyFareClass :: FareClass,
    rebookingAllowed :: Bool,
    changeFee :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp RebookingPolicy where
  disp p =
    vsep
      [ "Fare Class:" <+> disp p.policyFareClass,
        "Rebooking Allowed:" <+> (if p.rebookingAllowed then "Yes" else "No"),
        "Change Fee:" <+> pretty (formatCents p.changeFee)
      ]

--------------------------------------------------------------------------------
-- Database Type
--------------------------------------------------------------------------------

-- | The complete database for AirLogic Airlines.
data AirLogicDB = AirLogicDB
  { flights :: Map Text Flight,
    bookings :: Map Text Booking,
    users :: Map Text User,
    airports :: Map Text Airport,
    rebookingPolicies :: Map FareClass RebookingPolicy
  }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Format cents as dollars with proper zero-padding.
formatCents :: Int -> Text
formatCents cents =
  let dollars = cents `div` 100
      remainder = abs (cents `mod` 100)
   in "$" <> T.pack (show dollars) <> "." <> T.justifyRight 2 '0' (T.pack (show remainder))
