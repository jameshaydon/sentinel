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

    -- * Helpers
    extractString,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Examples.AirCanada.Facts qualified as Facts
import Examples.AirCanada.MockDB (attemptRefund, getBooking, getFlight, listBookingsForPassenger)
import Examples.AirCanada.Refund (BookingSource (..), DeathCircumstance (..), SpecialException (..))
import Examples.AirCanada.Types
import Pre
import Sentinel.Guard qualified as Guard
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel (getDb, modifyDb)
import Sentinel.Tool (Tool (..), ToolCategory (..), ToolOutput (..))
import Sentinel.Toolkit (Toolkit (..))

--------------------------------------------------------------------------------
-- Toolkit
--------------------------------------------------------------------------------

-- | The Air Canada toolkit with all tools and system prompt.
airCanadaToolkit :: Toolkit AirlineDB Facts.Fact
airCanadaToolkit =
  Toolkit
    { tools =
        [ loginTool,
          retrieveBookingTool,
          checkFlightTool,
          searchBookingsTool,
          processRefundTool
        ],
      systemPrompt = airCanadaSystemPrompt
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
loginTool :: Tool AirlineDB Facts.Fact
loginTool =
  Tool
    { name = "Login",
      description = "Log in a user by their name. This establishes the user's identity for the session, allowing them to access their bookings and perform actions.",
      params =
        Schema.objectSchema
          [("userName", Schema.stringProp "The user's full name")]
          ["userName"],
      category = ActionTool, -- Login is an action, not auto-invoked
      guard = \_ -> pure (), -- No guard for login - always allowed
      execute = \args -> do
        userName <- extractString "userName" args ??: "Missing or invalid 'userName' parameter"
        pure
          ToolOutput
            { observation = "Successfully logged in as: " <> userName,
              producedFacts = [Facts.LoggedInUser userName]
            }
    }

-- | Retrieve booking tool - looks up booking details.
retrieveBookingTool :: Tool AirlineDB Facts.Fact
retrieveBookingTool =
  Tool
    { name = "RetrieveBooking",
      description = "Look up booking details by booking reference (PNR). Use this to find passenger name, flight number, ticket class, and refund eligibility.",
      params =
        Schema.objectSchema
          [("bookingRef", Schema.stringProp "The 6-character booking reference (e.g., REF123)")]
          ["bookingRef"],
      category = DataTool,
      guard = userIdentityGuard,
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
checkFlightTool :: Tool AirlineDB Facts.Fact
checkFlightTool =
  Tool
    { name = "CheckFlightStatus",
      description = "Check if a flight is OnTime, Delayed, Cancelled, Boarding, or Landed. Always retrieve the booking first to get the flight number.",
      params =
        Schema.objectSchema
          [("flightNumber", Schema.stringProp "The flight number (e.g., AC101)")]
          ["flightNumber"],
      category = DataTool,
      guard = userIdentityGuard,
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
searchBookingsTool :: Tool AirlineDB Facts.Fact
searchBookingsTool =
  Tool
    { name = "SearchBookingsByName",
      description = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      params =
        Schema.objectSchema
          [("passengerName", Schema.stringProp "The passenger's full name")]
          ["passengerName"],
      category = DataTool,
      guard = searchBookingsGuard,
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
processRefundTool :: Tool AirlineDB Facts.Fact
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
      guard = refundGuard,
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
-- Guards
--------------------------------------------------------------------------------

-- | Guard that requires user identity to be established.
userIdentityGuard :: Aeson.Value -> Guard.GuardM AirlineDB Facts.Fact ()
userIdentityGuard _ = do
  facts <- Guard.queryFacts \case
    Facts.LoggedInUser _ -> True
    _ -> False
  case facts of
    (_ : _) -> pure ()
    [] -> Guard.denyWith "User is not logged in. Use the Login tool to establish user identity first."

-- | Guard for SearchBookingsByName - requires identity AND name must match logged-in user.
searchBookingsGuard :: Aeson.Value -> Guard.GuardM AirlineDB Facts.Fact ()
searchBookingsGuard args = do
  -- First check if user is logged in
  loggedInFacts <- Guard.queryFacts \case
    Facts.LoggedInUser _ -> True
    _ -> False
  case loggedInFacts of
    [] -> Guard.denyWith "User is not logged in. Use the Login tool to establish user identity first."
    (loggedInUser : _) -> do
      -- Extract the logged-in user's name
      let userName = case loggedInUser of
            Facts.LoggedInUser name -> name
            _ -> "" -- Won't happen due to pattern match above
      -- Verify the passengerName argument matches the logged-in user
      case extractString "passengerName" args of
        Nothing -> Guard.denyWith "Missing passengerName parameter"
        Just name
          | T.toUpper name == T.toUpper userName -> pure ()
          | otherwise -> Guard.denyWith "Can only search bookings for yourself"

-- | Guard for refund tool.
-- Requires: booking must exist in facts (auto-fetches if needed)
-- Forbids: booking from travel agency or other airline
refundGuard :: Aeson.Value -> Guard.GuardM AirlineDB Facts.Fact ()
refundGuard args = do
  -- First require user identity
  userIdentityGuard args

  -- Extract booking reference from args
  case extractString "bookingRef" args of
    Nothing -> Guard.denyWith "Missing or invalid 'bookingRef' parameter"
    Just ref -> do
      let normalizedRef = T.toUpper ref
      -- Try to establish that booking exists
      -- If not present, execute RetrieveBooking to fetch it
      Guard.establishFact
        (Facts.BookingExists normalizedRef)
        "RetrieveBooking"
        (Aeson.object [("bookingRef", Aeson.String ref)])

      -- Forbid bookings from travel agencies (we can't process those)
      Guard.forbidFactMatching "Booking is from a travel agency (cannot process refunds)" \case
        Facts.BookingSource bRef TravelAgency -> bRef == normalizedRef
        _ -> False

      -- Forbid bookings from other airlines (we can't process those)
      Guard.forbidFactMatching "Booking is from another airline (cannot process refunds)" \case
        Facts.BookingSource bRef OtherAirline -> bRef == normalizedRef
        _ -> False

--------------------------------------------------------------------------------
-- Fact Production
--------------------------------------------------------------------------------

-- | Convert a booking to its constituent facts.
bookingToFacts :: Booking -> [Facts.Fact]
bookingToFacts b =
  [ Facts.BookingExists b.bookingRef,
    Facts.BookingPassenger b.bookingRef b.passengerName,
    Facts.BookingFlight b.bookingRef b.flightNo,
    Facts.BookingStatus b.bookingRef b.bookingStatus,
    Facts.BookingSource b.bookingRef b.ticketDetails.bookingSource,
    Facts.BookingTicketType b.bookingRef b.ticketDetails.ticketType,
    Facts.BookingPriceCents b.bookingRef b.priceCents,
    Facts.BookingTicketClass b.bookingRef b.ticketClass
  ]

-- | Convert a flight to its constituent facts.
flightToFacts :: Flight -> [Facts.Fact]
flightToFacts f =
  [ Facts.FlightExists f.flightNumber,
    Facts.FlightStatusFact f.flightNumber f.status,
    Facts.FlightRoute f.flightNumber f.origin f.destination
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Helper to extract a string value from JSON args.
extractString :: Text -> Aeson.Value -> Maybe Text
extractString key (Aeson.Object obj) =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
extractString _ _ = Nothing
