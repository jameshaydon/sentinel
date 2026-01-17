module Examples.AirCanada.Tools
  ( airCanadaToolkit,
    airlineToolkit,
    executeTool,
  )
where

import Data.Text qualified as T
import Examples.AirCanada.MockDB
import Examples.AirCanada.Refund (DeathCircumstance (..), SpecialException (..))
import Examples.AirCanada.Types
import Pre
import Sentinel.Tool (Tool (..), ToolResult (..), Toolkit (..))

-- | The retrieve booking tool.
retrieveBookingTool :: Tool
retrieveBookingTool =
  Tool
    { toolName = "RetrieveBooking",
      toolDescription = "Look up booking details by booking reference (PNR). Use this to find passenger name, flight number, ticket class, and refund eligibility.",
      toolParameterDesc = "The 6-character booking reference (e.g., REF123)"
    }

-- | The check flight status tool.
checkFlightTool :: Tool
checkFlightTool =
  Tool
    { toolName = "CheckFlightStatus",
      toolDescription = "Check if a flight is OnTime, Delayed, Cancelled, Boarding, or Landed. Always retrieve the booking first to get the flight number.",
      toolParameterDesc = "The flight number (e.g., AC101)"
    }

-- | The initiate refund tool.
processRefundTool :: Tool
processRefundTool =
  Tool
    { toolName = "InitiateRefund",
      toolDescription =
        "Process a refund request. Automatically detects involuntary refund eligibility "
          <> "based on flight status (cancelled/delayed flights get full refunds). "
          <> "For special circumstances (jury duty, military orders, death), append the reason after a colon.",
      toolParameterDesc =
        "Booking reference, optionally with special reason: REF123 (auto-detect), "
          <> "REF123:jury (jury duty), REF123:military (military orders), REF123:death (death circumstances)"
    }

-- | The search bookings by passenger name tool.
searchBookingsTool :: Tool
searchBookingsTool =
  Tool
    { toolName = "SearchBookingsByName",
      toolDescription = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      toolParameterDesc = "The passenger's full name"
    }

airlineToolkit :: [Tool]
airlineToolkit =
  [ retrieveBookingTool,
    checkFlightTool,
    processRefundTool,
    searchBookingsTool
  ]

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

-- | Parse optional special exception from input like "REF123:jury".
parseRefundInput :: Text -> (Text, Maybe SpecialException)
parseRefundInput input =
  case T.splitOn ":" (T.strip input) of
    [ref] -> (T.toUpper ref, Nothing)
    [ref, reason] ->
      let specialReason = case T.toLower (T.strip reason) of
            "jury" -> Just JuryDuty
            "military" -> Just MilitaryOrders
            "death" -> Just (Death PassengerDeath)
            _ -> Nothing
       in (T.toUpper ref, specialReason)
    _ -> (T.toUpper $ T.strip input, Nothing)

executeTool :: Text -> Text -> AirlineDB -> ToolResult
executeTool toolName input db =
  case T.toLower $ T.strip toolName of
    "retrievebooking" ->
      case getBooking input db of
        Just booking -> ToolSuccess $ renderDocPlain (disp booking)
        Nothing -> ToolError $ "No booking found with reference: " <> input
    "checkflightstatus" ->
      case getFlight input db of
        Just flight -> ToolSuccess $ renderDocPlain (disp flight)
        Nothing -> ToolError $ "No flight found with number: " <> input
    "initiaterefund" ->
      let (ref, specialReason) = parseRefundInput input
       in ToolSuccess $ attemptRefund ref specialReason db
    "searchbookingsbyname" ->
      case listBookingsForPassenger input db of
        [] -> ToolError $ "No bookings found for passenger: " <> input
        bookings ->
          ToolSuccess
            $ renderDocPlain
            $ vsep
              [ "Found" <+> pretty (length bookings) <+> "booking(s):",
                mempty,
                vsep (punctuate (line <> "---" <> line) (fmap disp bookings))
              ]
    _ ->
      ToolError $ "Unknown tool: " <> toolName

airCanadaToolkit :: Toolkit AirlineDB
airCanadaToolkit =
  Toolkit
    { tools = airlineToolkit,
      executeTool = executeTool,
      systemPrompt = systemPrompt
    }
