-- | Mock database for the AirLogic Airlines example.
--
-- Contains sample data for two demo scenarios:
-- - Scenario A: Sarah Chen with a delayed flight (successful refund)
-- - Scenario B: Marcus Webb with an on-time Basic Economy flight (denied refund)
module Examples.AirLogic.MockDB
  ( -- * Database
    initialDB,

    -- * Query Functions
    getUserBookings,
    getBooking,
    getFlight,
    getAirportInfo,
    getUserProfile,
    getRebookingPolicy,
  )
where

import Data.Map.Strict qualified as M
import Data.Time (UTCTime (..), fromGregorian)
import Examples.AirLogic.Types
import Pre

--------------------------------------------------------------------------------
-- Initial Database
--------------------------------------------------------------------------------

-- | Initialize the mock database with sample data for both demo scenarios.
initialDB :: AirLogicDB
initialDB =
  AirLogicDB
    { flights = M.fromList [(f.flightId, f) | f <- allFlights],
      bookings = M.fromList [(b.bookingId, b) | b <- allBookings],
      users = M.fromList [(u.oduserId, u) | u <- allUsers],
      airports = M.fromList [(a.code, a) | a <- allAirports],
      rebookingPolicies = M.fromList [(p.policyFareClass, p) | p <- allRebookingPolicies]
    }

--------------------------------------------------------------------------------
-- Flights
--------------------------------------------------------------------------------

allFlights :: [Flight]
allFlights =
  [ -- Scenario A: Sarah Chen's delayed flight
    Flight
      { flightId = "AL-445",
        departureAirport = "LHR",
        arrivalAirport = "JFK",
        status = Delayed,
        delayMinutes = 360, -- 6 hours delay
        departureTime = UTCTime (fromGregorian 2025 1 15) (18 * 3600)
      },
    -- Scenario B: Marcus Webb's on-time flight
    Flight
      { flightId = "AL-892",
        departureAirport = "ORD",
        arrivalAirport = "LAX",
        status = OnTime,
        delayMinutes = 0,
        departureTime = UTCTime (fromGregorian 2025 1 16) (10 * 3600)
      }
  ]

--------------------------------------------------------------------------------
-- Bookings
--------------------------------------------------------------------------------

allBookings :: [Booking]
allBookings =
  [ -- Scenario A: Sarah Chen's Standard booking on delayed flight
    Booking
      { bookingId = "BK-2847",
        userId = "usr_sarah_chen",
        flightId = "AL-445",
        fareClass = Standard,
        totalAmountCents = 89200,
        hoursUntilDeparture = 4
      },
    -- Scenario B: Marcus Webb's Basic Economy booking
    Booking
      { bookingId = "BK-5521",
        userId = "usr_marcus_webb",
        flightId = "AL-892",
        fareClass = Basic,
        totalAmountCents = 28900,
        hoursUntilDeparture = 18
      }
  ]

--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

allUsers :: [User]
allUsers =
  [ -- Scenario A: Sarah Chen (Gold tier)
    User
      { oduserId = "usr_sarah_chen",
        name = "Sarah Chen",
        email = "sarah.chen@email.com",
        loyaltyTier = Gold
      },
    -- Scenario B: Marcus Webb (Bronze tier)
    User
      { oduserId = "usr_marcus_webb",
        name = "Marcus Webb",
        email = "marcus.webb@email.com",
        loyaltyTier = Bronze
      }
  ]

--------------------------------------------------------------------------------
-- Airports
--------------------------------------------------------------------------------

allAirports :: [Airport]
allAirports =
  [ -- EU airports (for EU261 eligibility)
    Airport
      { code = "LHR",
        name = "London Heathrow",
        isEU = True, -- UK airports still in EU261 scheme for flights departing UK
        country = "United Kingdom"
      },
    Airport
      { code = "CDG",
        name = "Paris Charles de Gaulle",
        isEU = True,
        country = "France"
      },
    Airport
      { code = "FRA",
        name = "Frankfurt Airport",
        isEU = True,
        country = "Germany"
      },
    -- Non-EU airports
    Airport
      { code = "JFK",
        name = "John F. Kennedy International",
        isEU = False,
        country = "United States"
      },
    Airport
      { code = "ORD",
        name = "O'Hare International",
        isEU = False,
        country = "United States"
      },
    Airport
      { code = "LAX",
        name = "Los Angeles International",
        isEU = False,
        country = "United States"
      }
  ]

--------------------------------------------------------------------------------
-- Rebooking Policies
--------------------------------------------------------------------------------

allRebookingPolicies :: [RebookingPolicy]
allRebookingPolicies =
  [ -- Flexible: no fee
    RebookingPolicy
      { policyFareClass = Flexible,
        rebookingAllowed = True,
        changeFee = 0
      },
    -- Standard: $100 fee
    RebookingPolicy
      { policyFareClass = Standard,
        rebookingAllowed = True,
        changeFee = 10000
      },
    -- Basic: $75 fee
    RebookingPolicy
      { policyFareClass = Basic,
        rebookingAllowed = True,
        changeFee = 7500
      }
  ]

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- | Get all bookings for a user.
getUserBookings :: Text -> AirLogicDB -> [Booking]
getUserBookings uid db =
  filter (\b -> b.userId == uid) (M.elems db.bookings)

-- | Get a booking by ID.
getBooking :: Text -> AirLogicDB -> Maybe Booking
getBooking bookingId db = M.lookup bookingId db.bookings

-- | Get a flight by ID.
getFlight :: Text -> AirLogicDB -> Maybe Flight
getFlight flightId db = M.lookup flightId db.flights

-- | Get airport info by code.
getAirportInfo :: Text -> AirLogicDB -> Maybe Airport
getAirportInfo code db = M.lookup code db.airports

-- | Get user profile by ID.
getUserProfile :: Text -> AirLogicDB -> Maybe User
getUserProfile uid db = M.lookup uid db.users

-- | Get rebooking policy by fare class.
getRebookingPolicy :: FareClass -> AirLogicDB -> Maybe RebookingPolicy
getRebookingPolicy fc db = M.lookup fc db.rebookingPolicies
