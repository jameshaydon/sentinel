-- | Air Canada tool definitions with embedded guards and execution logic.
--
-- Each tool is a self-contained definition including:
-- - Metadata for the LLM (name, description, params)
-- - Category (data vs action)
-- - Guard (precondition for execution)
-- - Execute (implementation returning observation + facts)
module Examples.AirCanada.Tools
  ( -- * Toolkit
    airCanadaToolkit,
    airCanadaSystemPrompt,

    -- * Individual Tools
    loginTool,
    retrieveBookingTool,
    checkFlightTool,
    searchBookingsTool,
    processRefundTool,

    -- * Helpers (re-exported from Sentinel.JSON)
    extractString,

    -- * Fact Production (exported for tests)
    bookingToFacts,
    flightToFacts,
  )
where

import Data.Text qualified as T
import Examples.AirCanada.MockDB (attemptRefund, getBooking, getFlight, listBookingsForPassenger)
import Examples.AirCanada.Refund (DeathCircumstance (..), SpecialException (..))
import Examples.AirCanada.ToolBindings (airCanadaToolBindings)
import Examples.AirCanada.Types
import Pre
import Sentinel.Context (emptyContextDecls)
import Sentinel.JSON (extractString)
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel (getDb, modifyDb)
import Sentinel.Solver.Askable (emptyAskableRegistry)
import Sentinel.Solver.Combinators (extractArg, oneOf, queryPredicate)
import Sentinel.Solver.Types (BaseFact (..), Proof (..), Scalar (..))
import Sentinel.Tool (Guard, Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))
import Sentinel.Toolkit (Toolkit (..))

--------------------------------------------------------------------------------
-- Toolkit
--------------------------------------------------------------------------------

-- | The Air Canada toolkit with all tools and system prompt.
airCanadaToolkit :: Toolkit AirlineDB
airCanadaToolkit =
  Toolkit
    { tools =
        [ loginTool,
          retrieveBookingTool,
          checkFlightTool,
          searchBookingsTool,
          processRefundTool
        ],
      systemPrompt = airCanadaSystemPrompt,
      toolBindings = airCanadaToolBindings,
      askables = emptyAskableRegistry,
      contextDecls = emptyContextDecls
    }

--------------------------------------------------------------------------------
-- System Prompt
--------------------------------------------------------------------------------

-- | System prompt for the Air Canada agent.
airCanadaSystemPrompt :: Text
airCanadaSystemPrompt =
  T.unlines
    [ "You are a helpful customer service agent for Air Canada.",
      "You help customers with flight information, booking inquiries, and refund requests.",
      "",
      "IMPORTANT INSTRUCTIONS:",
      "1. Always retrieve booking information before checking flight status",
      "2. Always verify refund eligibility before processing a refund",
      "3. Be helpful and empathetic with customers",
      "4. When you have enough information, provide a clear and helpful response",
      "",
      "Always be polite and professional."
    ]

--------------------------------------------------------------------------------
-- Tools
--------------------------------------------------------------------------------

-- | Login tool - establishes user identity.
loginTool :: Tool AirlineDB
loginTool =
  Tool
    { name = "Login",
      description = "Log in a user by their name. This establishes the user's identity for the session, allowing them to access their bookings and perform actions.",
      params =
        Schema.objectSchema
          [("userName", Schema.stringProp "The user's full name")]
          ["userName"],
      category = ActionTool, -- Login is an action, not auto-invoked
      guard = NoGuard, -- No guard for login - always allowed
      execute = \args -> do
        userName <- extractString "userName" args ??: "Missing or invalid 'userName' parameter"
        pure
          ToolOutput
            { observation = "Successfully logged in as: " <> userName,
              producedFacts = [BaseFact "logged_in_user" [ScStr userName]]
            }
    }

-- | Retrieve booking tool - looks up booking details.
retrieveBookingTool :: Tool AirlineDB
retrieveBookingTool =
  Tool
    { name = "RetrieveBooking",
      description = "Look up booking details by booking reference (PNR). Use this to find passenger name, flight number, ticket class, and refund eligibility.",
      params =
        Schema.objectSchema
          [("bookingRef", Schema.stringProp "The 6-character booking reference (e.g., REF123)")]
          ["bookingRef"],
      category = DataTool,
      guard = SolverGuardT "user_identity" userIdentityGuard,
      execute = \args -> do
        ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
        db <- lift getDb
        case getBooking ref db of
          Just booking ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp booking),
                  producedFacts = bookingToFacts booking
                }
          Nothing -> throwError $ "No booking found with reference: " <> ref
    }

-- | Check flight status tool.
checkFlightTool :: Tool AirlineDB
checkFlightTool =
  Tool
    { name = "CheckFlightStatus",
      description = "Check if a flight is OnTime, Delayed, Cancelled, Boarding, or Landed. Always retrieve the booking first to get the flight number.",
      params =
        Schema.objectSchema
          [("flightNumber", Schema.stringProp "The flight number (e.g., AC101)")]
          ["flightNumber"],
      category = DataTool,
      guard = SolverGuardT "user_identity" userIdentityGuard,
      execute = \args -> do
        flightNum <- extractString "flightNumber" args ??: "Missing or invalid 'flightNumber' parameter"
        db <- lift getDb
        case getFlight flightNum db of
          Just flight ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp flight),
                  producedFacts = flightToFacts flight
                }
          Nothing -> throwError $ "No flight found with number: " <> flightNum
    }

-- | Search bookings by passenger name tool.
searchBookingsTool :: Tool AirlineDB
searchBookingsTool =
  Tool
    { name = "SearchBookingsByName",
      description = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      params =
        Schema.objectSchema
          [("passengerName", Schema.stringProp "The passenger's full name")]
          ["passengerName"],
      category = DataTool,
      guard = SolverGuardT "search_bookings" searchBookingsGuard,
      execute = \args -> do
        name <- extractString "passengerName" args ??: "Missing or invalid 'passengerName' parameter"
        db <- lift getDb
        case listBookingsForPassenger name db of
          [] -> throwError $ "No bookings found for passenger: " <> name
          bookings ->
            pure
              ToolOutput
                { observation =
                    renderDocPlain
                      $ vsep
                        [ "Found" <+> pretty (length bookings) <+> "booking(s):",
                          mempty,
                          vsep (punctuate (line <> "---" <> line) (fmap disp bookings))
                        ],
                  producedFacts = concatMap bookingToFacts bookings
                }
    }

-- | Process refund tool.
processRefundTool :: Tool AirlineDB
processRefundTool =
  Tool
    { name = "InitiateRefund",
      description =
        "Process a refund request. Automatically detects involuntary refund eligibility "
          <> "based on flight status (cancelled/delayed flights get full refunds). "
          <> "For special circumstances, provide the reason parameter.",
      params =
        Schema.objectSchema
          [ ("bookingRef", Schema.stringProp "The 6-character booking reference"),
            ("reason", Schema.enumProp ["jury", "military", "death"] "Optional special circumstance reason: jury (jury duty), military (military orders), death (death circumstances)")
          ]
          ["bookingRef"],
      category = ActionTool,
      guard = SolverGuardT "refund_eligibility" refundGuard,
      execute = \args -> do
        ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
        let specialReason = case extractString "reason" args of
              Just "jury" -> Just JuryDuty
              Just "military" -> Just MilitaryOrders
              Just "death" -> Just (Death PassengerDeath)
              _ -> Nothing
        db <- lift getDb
        let (result, updatedDB) = attemptRefund (T.toUpper ref) specialReason db
        lift $ modifyDb (const updatedDB)
        pure
          ToolOutput
            { observation = result,
              producedFacts = [] -- Action tools produce no facts
            }
    }

--------------------------------------------------------------------------------
-- Guards (Function-based)
--------------------------------------------------------------------------------

-- | Guard that requires user identity to be established.
--
-- Queries the "logged_in_user" predicate to verify a user is logged in.
userIdentityGuard :: Guard
userIdentityGuard _args = do
  fact <- queryPredicate "logged_in_user" []
  pure $ FactUsed fact

-- | Guard for SearchBookingsByName - requires identity AND name must match logged-in user.
--
-- This guard:
-- 1. Requires a user to be logged in
-- 2. Verifies the passengerName argument matches the logged-in user
searchBookingsGuard :: Guard
searchBookingsGuard args = do
  name <- extractArg "passengerName" args
  fact <- queryPredicate "logged_in_user" [name]
  pure $ FactUsed fact

-- | Guard for refund tool.
--
-- Requires:
-- - User identity established
-- - Booking must exist (will be auto-fetched via tool binding)
-- - Booking source must not be TravelAgency or OtherAirline
refundGuard :: Guard
refundGuard args = do
  bookingRef <- extractArg "bookingRef" args
  -- Require user identity
  _ <- queryPredicate "logged_in_user" []
  -- Booking must exist (auto-fetched via tool binding)
  _ <- queryPredicate "booking_passenger" [bookingRef]
  -- Booking source must be acceptable (not from travel agency or other airline)
  oneOf
    [ do
        fact <- queryPredicate "booking_source" [bookingRef, ScStr "DirectAirCanada"]
        pure $ FactUsed fact,
      do
        fact <- queryPredicate "booking_source" [bookingRef, ScStr "GroupBooking"]
        pure $ FactUsed fact
    ]

--------------------------------------------------------------------------------
-- Fact Production
--------------------------------------------------------------------------------

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
