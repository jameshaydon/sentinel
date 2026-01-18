-- | Air Canada tool definitions for the LLM.
--
-- This module contains only the tool metadata (name, description, params)
-- that the Agent passes to the LLM for tool calling. The actual tool
-- execution and guard evaluation is handled by Examples.AirCanada.Sentinel.
module Examples.AirCanada.Tools
  ( airCanadaTools,
    airCanadaSystemPrompt,
  )
where

import Data.Text qualified as T
import Pre
import Sentinel.Schema qualified as Schema
import Sentinel.Tool (Tool (..))

--------------------------------------------------------------------------------
-- Tool Definitions
--------------------------------------------------------------------------------

-- | The retrieve booking tool.
retrieveBookingTool :: Tool
retrieveBookingTool =
  Tool
    { toolName = "RetrieveBooking",
      toolDescription = "Look up booking details by booking reference (PNR). Use this to find passenger name, flight number, ticket class, and refund eligibility.",
      toolParams =
        Schema.objectSchema
          [("bookingRef", Schema.stringProp "The 6-character booking reference (e.g., REF123)")]
          ["bookingRef"]
    }

-- | The check flight status tool.
checkFlightTool :: Tool
checkFlightTool =
  Tool
    { toolName = "CheckFlightStatus",
      toolDescription = "Check if a flight is OnTime, Delayed, Cancelled, Boarding, or Landed. Always retrieve the booking first to get the flight number.",
      toolParams =
        Schema.objectSchema
          [("flightNumber", Schema.stringProp "The flight number (e.g., AC101)")]
          ["flightNumber"]
    }

-- | The initiate refund tool.
processRefundTool :: Tool
processRefundTool =
  Tool
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
          ["bookingRef"]
    }

-- | The search bookings by passenger name tool.
searchBookingsTool :: Tool
searchBookingsTool =
  Tool
    { toolName = "SearchBookingsByName",
      toolDescription = "Search for all bookings associated with a passenger name. Use this when the customer provides their name but not a booking reference.",
      toolParams =
        Schema.objectSchema
          [("passengerName", Schema.stringProp "The passenger's full name")]
          ["passengerName"]
    }

-- | The login tool - establishes user identity.
loginTool :: Tool
loginTool =
  Tool
    { toolName = "Login",
      toolDescription = "Log in a user by their name. This establishes the user's identity for the session, allowing them to access their bookings and perform actions.",
      toolParams =
        Schema.objectSchema
          [("userName", Schema.stringProp "The user's full name")]
          ["userName"]
    }

--------------------------------------------------------------------------------
-- Exported Collections
--------------------------------------------------------------------------------

-- | Simple tool metadata for the Agent/LLM.
airCanadaTools :: [Tool]
airCanadaTools =
  [ loginTool,
    retrieveBookingTool,
    checkFlightTool,
    processRefundTool,
    searchBookingsTool
  ]

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
