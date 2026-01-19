-- | Context variable declarations for the Air Canada example.
--
-- Context variables are rebindable references that track conversational focus.
-- They solve the problem of "which booking are we talking about?" while keeping
-- queries purely symbolic. The LLM can rebind these when conversational focus
-- shifts (e.g., "actually, what about my other booking?").
module Examples.AirCanada.ContextDecls
  ( airCanadaContextDecls,
  )
where

import Pre
import Sentinel.Context
  ( ContextDecl (..),
    ContextDecls,
    SeedSpec (..),
    declareContext,
    emptyContextDecls,
  )

-- | Context variable declarations for Air Canada.
airCanadaContextDecls :: ContextDecls
airCanadaContextDecls =
  emptyContextDecls
    & declareContext currentUserDecl
    & declareContext bookingOfInterestDecl
    & declareContext flightOfInterestDecl

--------------------------------------------------------------------------------
-- Context Declarations
--------------------------------------------------------------------------------

-- | The currently logged-in user.
--
-- This context variable is pre-seeded from the session when the agent
-- initializes. It identifies the authenticated user making requests.
--
-- Context: current_user
currentUserDecl :: ContextDecl
currentUserDecl =
  ContextDecl
    { name = "current_user",
      candidateQuery = Nothing, -- No query - must be seeded from session
      seedValue = Just (FromSession "user_id"),
      description = "The authenticated user making requests"
    }

-- | The booking the user is asking about.
--
-- This context variable tracks which booking is currently being discussed.
-- When the solver needs this context but it's not established, it blocks
-- and returns candidates from the user's bookings.
--
-- Context: booking_of_interest
-- Candidate query: user_bookings(CurrentUser, BookingId) - returns user's bookings
bookingOfInterestDecl :: ContextDecl
bookingOfInterestDecl =
  ContextDecl
    { name = "booking_of_interest",
      candidateQuery = Just "user_bookings", -- Query to find candidates
      seedValue = Nothing, -- Not seeded - user must select
      description = "The booking the customer is asking about"
    }

-- | The flight associated with the booking of interest.
--
-- This context variable is derived from booking_of_interest. Once a booking
-- is established, the flight can be looked up via the booking_flight predicate.
--
-- Context: flight_of_interest
-- Derived from: booking_flight(booking_of_interest, F)
flightOfInterestDecl :: ContextDecl
flightOfInterestDecl =
  ContextDecl
    { name = "flight_of_interest",
      candidateQuery = Just "booking_flight", -- Derived from booking
      seedValue = Nothing, -- Derived, not directly seeded
      description = "The flight for the booking of interest"
    }
