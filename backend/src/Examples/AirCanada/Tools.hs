module Examples.AirCanada.Tools
  ( airCanadaToolkit,
  )
where

import Control.Monad.State.Strict (get, modify)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Examples.AirCanada.Facts qualified as Facts
import Examples.AirCanada.MockDB (attemptRefund, getBooking, getFlight, listBookingsForPassenger)
import Examples.AirCanada.Refund (BookingSource (..), DeathCircumstance (..), SpecialException (..))
import Examples.AirCanada.Types
import Pre
import Sentinel.Agent (AgentM, AgentState (..))
import Sentinel.Guard qualified as Guard
import Sentinel.Schema qualified as Schema
import Sentinel.Tool (ToolKind (..))
import Sentinel.Tool qualified as Tool

-- | Type alias for Air Canada agent monad
type AirCanadaM = AgentM AirlineDB Facts.Fact

-- | Type alias for Air Canada tools
type AirCanadaTool = Tool.Tool AirCanadaM Facts.Fact

-- | The retrieve booking tool.
retrieveBookingTool :: AirCanadaTool
retrieveBookingTool =
  Tool.Tool
    { toolName = "RetrieveBooking",
      toolDescription = "Look up booking details by booking reference (PNR). Use this to find passenger name, flight number, ticket class, and refund eligibility.",
      toolParams =
        Schema.objectSchema
          [("bookingRef", Schema.stringProp "The 6-character booking reference (e.g., REF123)")]
          ["bookingRef"],
      toolKind = QueryTool,
      toolGuard = userIdentityGuard,
      toolAction = \args -> do
        ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
        st <- lift get
        case getBooking ref st.db of
          Just booking -> pure $ renderDocPlain (disp booking)
          Nothing -> throwError $ "No booking found with reference: " <> ref,
      toolFactsProduced = \args -> do
        st <- get
        pure $ case extractString "bookingRef" args of
          Nothing -> []
          Just ref -> case getBooking ref st.db of
            Nothing -> []
            Just booking -> bookingToFacts booking
    }

-- | The check flight status tool.
checkFlightTool :: AirCanadaTool
checkFlightTool =
  Tool.Tool
    { toolName = "CheckFlightStatus",
      toolDescription = "Check if a flight is OnTime, Delayed, Cancelled, Boarding, or Landed. Always retrieve the booking first to get the flight number.",
      toolParams =
        Schema.objectSchema
          [("flightNumber", Schema.stringProp "The flight number (e.g., AC101)")]
          ["flightNumber"],
      toolKind = QueryTool,
      toolGuard = userIdentityGuard,
      toolAction = \args -> do
        flightNum <- extractString "flightNumber" args ??: "Missing or invalid 'flightNumber' parameter"
        st <- lift get
        case getFlight flightNum st.db of
          Just flight -> pure $ renderDocPlain (disp flight)
          Nothing -> throwError $ "No flight found with number: " <> flightNum,
      toolFactsProduced = \args -> do
        st <- get
        pure $ case extractString "flightNumber" args of
          Nothing -> []
          Just flightNum -> case getFlight flightNum st.db of
            Nothing -> []
            Just flight -> flightToFacts flight
    }

-- | The initiate refund tool.
processRefundTool :: AirCanadaTool
processRefundTool =
  Tool.Tool
    { toolName = "InitiateRefund",
      toolDescription =
        "Process a refund request. Automatically detects involuntary refund eligibility "
          <> "based on flight status (cancelled/delayed flights get full refunds). "
          <> "For special circumstances, provide the reason parameter.",
      toolParams =
        Schema.objectSchema
          [ ("bookingRef", Schema.stringProp "The 6-character booking reference"),
            ("reason", Schema.enumProp ["jury", "military", "death"] "Optional special circumstance reason: jury (jury duty), military (military orders), death (death circumstances)")
          ]
          ["bookingRef"],
      toolKind = ActionTool,
      toolGuard = refundGuard,
      toolAction = \args -> do
        ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
        let specialReason = case extractString "reason" args of
              Just "jury" -> Just JuryDuty
              Just "military" -> Just MilitaryOrders
              Just "death" -> Just (Death PassengerDeath)
              _ -> Nothing
        st <- lift get
        let (result, updatedDB) = attemptRefund (T.toUpper ref) specialReason st.db
        lift $ modify \s -> s {db = updatedDB}
        pure result,
      toolFactsProduced = \_ -> pure [] -- Action tools produce no facts
    }

-- | Guard that requires user identity to be established.
-- All query tools should use this to ensure we know who the user is.
userIdentityGuard :: Aeson.Value -> Guard.GuardM Facts.Fact ()
userIdentityGuard _ = do
  -- Require that we have established who the user is
  _ <- Guard.requireFactMatching \case
    Facts.LoggedInUser _ -> True
    _ -> False
  pure ()

-- | Guard for SearchBookingsByName - requires identity AND name must match logged-in user.
searchBookingsGuard :: Aeson.Value -> Guard.GuardM Facts.Fact ()
searchBookingsGuard args = do
  -- First require user identity
  loggedInUser <- Guard.requireFactMatching \case
    Facts.LoggedInUser _ -> True
    _ -> False
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

-- | Guard for refund tool using LogicT.
-- Requires: booking must exist in facts
-- Forbids: booking from travel agency or other airline
refundGuard :: Aeson.Value -> Guard.GuardM Facts.Fact ()
refundGuard args = do
  -- Extract booking reference from args
  let mRef = extractString "bookingRef" args
  case mRef of
    Nothing -> Guard.denyWith "Missing or invalid 'bookingRef' parameter"
    Just ref -> do
      let normalizedRef = T.toUpper ref
      -- Try to establish that booking exists
      -- If not present, register a query that could establish it
      Guard.tryEstablishFact
        (Facts.BookingExists normalizedRef)
        ( Guard.PendingQuery
            { queryToolName = "RetrieveBooking",
              queryArgs = Aeson.object [("bookingRef", Aeson.String ref)],
              queryDescription = "Retrieve booking to verify it exists"
            }
        )
      -- Forbid bookings from travel agencies (we can't process those)
      Guard.forbidFactMatching \case
        Facts.BookingSource bRef TravelAgency -> bRef == normalizedRef
        _ -> False
      -- Forbid bookings from other airlines (we can't process those)
      Guard.forbidFactMatching \case
        Facts.BookingSource bRef OtherAirline -> bRef == normalizedRef
        _ -> False

-- | The search bookings by passenger name tool.
searchBookingsTool :: AirCanadaTool
searchBookingsTool =
  Tool.Tool
    { toolName = "SearchBookingsByName",
      toolDescription = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      toolParams =
        Schema.objectSchema
          [("passengerName", Schema.stringProp "The passenger's full name")]
          ["passengerName"],
      toolKind = QueryTool,
      toolGuard = searchBookingsGuard,
      toolAction = \args -> do
        name <- extractString "passengerName" args ??: "Missing or invalid 'passengerName' parameter"
        st <- lift get
        case listBookingsForPassenger name st.db of
          [] -> throwError $ "No bookings found for passenger: " <> name
          bookings ->
            pure
              $ renderDocPlain
              $ vsep
                [ "Found" <+> pretty (length bookings) <+> "booking(s):",
                  mempty,
                  vsep (punctuate (line <> "---" <> line) (fmap disp bookings))
                ],
      toolFactsProduced = \args -> do
        st <- get
        pure $ case extractString "passengerName" args of
          Nothing -> []
          Just name -> concatMap bookingToFacts (listBookingsForPassenger name st.db)
    }

-- | Helper to extract a string value from JSON args.
extractString :: Text -> Aeson.Value -> Maybe Text
extractString key (Aeson.Object obj) =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
extractString _ _ = Nothing

--------------------------------------------------------------------------------
-- Fact Production Helpers
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
-- System Prompt
--------------------------------------------------------------------------------

systemPrompt :: Text
systemPrompt =
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

airCanadaToolkit :: Tool.Toolkit AirCanadaM Facts.Fact
airCanadaToolkit =
  Tool.Toolkit
    { tools =
        [ retrieveBookingTool,
          checkFlightTool,
          processRefundTool,
          searchBookingsTool
        ],
      systemPrompt = systemPrompt
    }
