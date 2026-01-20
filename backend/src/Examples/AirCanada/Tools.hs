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
import Examples.AirCanada.Facts (bookingToFacts, flightToFacts)
import Examples.AirCanada.Guards (refundGuard, searchBookingsGuard, userIdentityGuard)
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
import Sentinel.Solver.Types (BaseFact (..), Scalar (..))
import Sentinel.Tool (Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))
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
