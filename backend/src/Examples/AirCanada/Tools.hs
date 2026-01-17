module Examples.AirCanada.Tools
  ( airCanadaToolkit,
  )
where

import Control.Monad.State.Strict (get, modify)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Examples.AirCanada.MockDB (attemptRefund, checkRefundEligibility, getBooking, getFlight, listBookingsForPassenger)
import Examples.AirCanada.Refund (DeathCircumstance (..), SpecialException (..))
import Examples.AirCanada.Types
import Pre
import Sentinel.Agent (AgentM, AgentState (..))
import Sentinel.Schema qualified as Schema
import Sentinel.Tool qualified as Tool

-- | The retrieve booking tool.
retrieveBookingTool :: Tool.Tool (AgentM AirlineDB)
retrieveBookingTool =
  Tool.Tool
    { toolName = "RetrieveBooking",
      toolDescription = "Look up booking details by booking reference (PNR). Use this to find passenger name, flight number, ticket class, and refund eligibility.",
      toolParams =
        Schema.objectSchema
          [("bookingRef", Schema.stringProp "The 6-character booking reference (e.g., REF123)")]
          ["bookingRef"],
      toolGuard = \_ -> pure Nothing,
      toolAction = \args -> do
        ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
        st <- lift get
        case getBooking ref st.db of
          Just booking -> pure $ renderDocPlain (disp booking)
          Nothing -> throwError $ "No booking found with reference: " <> ref
    }

-- | The check flight status tool.
checkFlightTool :: Tool.Tool (AgentM AirlineDB)
checkFlightTool =
  Tool.Tool
    { toolName = "CheckFlightStatus",
      toolDescription = "Check if a flight is OnTime, Delayed, Cancelled, Boarding, or Landed. Always retrieve the booking first to get the flight number.",
      toolParams =
        Schema.objectSchema
          [("flightNumber", Schema.stringProp "The flight number (e.g., AC101)")]
          ["flightNumber"],
      toolGuard = \_ -> pure Nothing,
      toolAction = \args -> do
        flightNum <- extractString "flightNumber" args ??: "Missing or invalid 'flightNumber' parameter"
        st <- lift get
        case getFlight flightNum st.db of
          Just flight -> pure $ renderDocPlain (disp flight)
          Nothing -> throwError $ "No flight found with number: " <> flightNum
    }

-- | The initiate refund tool.
processRefundTool :: Tool.Tool (AgentM AirlineDB)
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
      toolGuard = \args -> do
        let mRef = extractString "bookingRef" args
        case mRef of
          Nothing -> pure $ Just "Missing or invalid 'bookingRef' parameter"
          Just ref -> do
            let specialReason = case extractString "reason" args of
                  Just "jury" -> Just JuryDuty
                  Just "military" -> Just MilitaryOrders
                  Just "death" -> Just (Death PassengerDeath)
                  _ -> Nothing
            st <- get
            pure $ checkRefundEligibility (T.toUpper ref) specialReason st.db,
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
        pure result
    }

-- | The search bookings by passenger name tool.
searchBookingsTool :: Tool.Tool (AgentM AirlineDB)
searchBookingsTool =
  Tool.Tool
    { toolName = "SearchBookingsByName",
      toolDescription = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      toolParams =
        Schema.objectSchema
          [("passengerName", Schema.stringProp "The passenger's full name")]
          ["passengerName"],
      toolGuard = \_ -> pure Nothing,
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
                ]
    }

-- | Helper to extract a string value from JSON args.
extractString :: Text -> Aeson.Value -> Maybe Text
extractString key (Aeson.Object obj) =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
extractString _ _ = Nothing

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

airCanadaToolkit :: Tool.Toolkit (AgentM AirlineDB)
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
