module Examples.AirCanada.Tools
  ( airCanadaToolkit,
    airlineToolkit,
    executeTool,
  )
where

import Data.Text qualified as T
import Examples.AirCanada.MockDB
import Examples.AirCanada.Types
import Pre
import Sentinel.Tool

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
      toolDescription = "Process a refund request. Only use this if the customer explicitly requests a refund AND after verifying the booking is eligible.",
      toolParameterDesc = "The booking reference to refund (e.g., REF123)"
    }

-- | The search bookings by passenger name tool.
searchBookingsTool :: Tool
searchBookingsTool =
  Tool
    { toolName = "SearchBookingsByName",
      toolDescription = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      toolParameterDesc = "The passenger's full name"
    }

-- | All available tools for the Air Canada agent.
airlineToolkit :: [Tool]
airlineToolkit =
  [ retrieveBookingTool,
    checkFlightTool,
    processRefundTool,
    searchBookingsTool
  ]

-- | The system prompt that defines the Air Canada agent's behavior.
systemPrompt :: Text
systemPrompt =
  T.unlines
    [ "You are a helpful customer service agent for Air Canada.",
      "You help customers with flight information, booking inquiries, and refund requests.",
      "",
      "You have access to the following tools:",
      "",
      toolsDescription airlineToolkit,
      "",
      "IMPORTANT INSTRUCTIONS:",
      "1. Always retrieve booking information before checking flight status",
      "2. Always verify refund eligibility before processing a refund",
      "3. Be helpful and empathetic with customers",
      "4. If you need to use a tool, respond with EXACTLY this format:",
      "",
      "Thought: [your reasoning about what to do next]",
      "Action: [tool name]",
      "Action Input: [the input to the tool]",
      "",
      "5. After receiving an observation, continue reasoning until you can give a final answer",
      "6. When you have enough information to answer, respond with:",
      "",
      "Thought: [your final reasoning]",
      "Final Answer: [your response to the customer]",
      "",
      "Always be polite and professional."
    ]

-- | Execute a tool by name with given input.
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
      ToolSuccess $ attemptRefund input db
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

-- | The complete Air Canada toolkit for use with the generic agent.
airCanadaToolkit :: Toolkit AirlineDB
airCanadaToolkit =
  Toolkit
    { tools = airlineToolkit,
      executeTool = executeTool,
      systemPrompt = systemPrompt
    }
