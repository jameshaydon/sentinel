-- | Air Canada tool definitions with embedded guards, askables, context, and execution logic.
--
-- Each tool is a self-contained definition including:
-- - Metadata for the LLM (name, description, params)
-- - Category (data vs action)
-- - Guard (precondition for execution)
-- - Execute (implementation returning observation + facts)
--
-- This module consolidates all Air Canada-specific components:
-- - Tools (data and action)
-- - Guards (with sophisticated solver-rule proofs)
-- - Askable declarations
-- - Context declarations
-- - Query tools
-- - Fact production
module Examples.AirCanada.Tools
  ( -- * Toolkit
    airCanadaToolkit,
    airCanadaSystemPrompt,

    -- * Data Tools
    loginTool,
    retrieveBookingTool,
    checkFlightTool,
    searchBookingsTool,

    -- * Action Tools
    processRefundTool,

    -- * Query Tools
    queryEligibilityTool,
    establishContextTool,

    -- * Askables
    airCanadaAskables,

    -- * Context
    airCanadaContextDecls,

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
import Sentinel.Context (ContextDecl (..), ContextDecls, SeedSpec (..), declareContext, emptyContextDecls)
import Sentinel.JSON (extractString)
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel (getDb, modifyDb, setContextValue)
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry, EvidenceType (..), declareAskable, emptyAskableRegistry)
import Sentinel.Solver.Combinators (SolverM, andAll, askable, contextVar, extractArg, oneOf, queryPredicate, require)
import Sentinel.Solver.Types (BaseFact (..), Proof (..), Scalar (..))
import Sentinel.Tool (Guard, Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))
import Sentinel.Toolkit (Toolkit (..), askUserAskableTool)

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
          processRefundTool,
          queryEligibilityTool,
          establishContextTool,
          askUserAskableTool
        ],
      systemPrompt = airCanadaSystemPrompt,
      toolBindings = airCanadaToolBindings,
      askables = airCanadaAskables,
      contextDecls = airCanadaContextDecls
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
      "REFUND POLICY SUMMARY:",
      "1. Airline-Caused Disruptions: Full refund if flight cancelled or significantly delayed",
      "2. EU261 Protection: Full refund if EU departure and significant delay",
      "3. Flexible/Business Fare: Full refund allowed",
      "4. Standard/Economy Fare: Partial refund (with cancellation penalty)",
      "5. Basic Economy: No cash refund, but voucher with medical documentation",
      "6. Bereavement: Partial refund with death certificate",
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
-- Data Tools
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

--------------------------------------------------------------------------------
-- Action Tools
--------------------------------------------------------------------------------

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
-- Query Tools
--------------------------------------------------------------------------------

-- | Tool for querying refund eligibility.
--
-- This is an informational query tool. The LLM uses it to check what
-- refund options are available before attempting to process a refund.
queryEligibilityTool :: Tool db
queryEligibilityTool =
  Tool
    { name = "QueryRefundEligibility",
      description =
        "Check if the current booking is eligible for a refund. "
          <> "Returns available refund options (full, partial, voucher) with reasons. "
          <> "May indicate that context needs to be established (which booking?) "
          <> "or that user confirmation is needed.",
      params =
        Schema.objectSchema
          [] -- No parameters - uses booking_of_interest context
          [],
      category = DataTool,
      guard = NoGuard, -- Query tools have no guard
      execute = \_args -> do
        -- This is a placeholder. The actual implementation requires access
        -- to the SolverEnv which is managed at the Agent level.
        pure
          ToolOutput
            { observation =
                "QueryRefundEligibility requires solver integration. "
                  <> "The Agent should run the eligibleForRefund rule via the solver.",
              producedFacts = []
            }
    }

-- | Tool for establishing a context variable.
--
-- When the solver blocks on a context variable (e.g., @booking_of_interest@),
-- the LLM asks the user to select a value and then calls this tool to
-- establish it.
establishContextTool :: Tool db
establishContextTool =
  Tool
    { name = "EstablishContext",
      description =
        "Set a context variable to a specific value. "
          <> "Use this after asking the user which booking they're asking about. "
          <> "Example: EstablishContext(slot='booking_of_interest', value='REF123')",
      params =
        Schema.objectSchema
          [ ("slot", Schema.stringProp "The context variable name (e.g., 'booking_of_interest')"),
            ("value", Schema.stringProp "The value to set (e.g., 'REF123')")
          ]
          ["slot", "value"],
      category = ActionTool, -- Context changes are actions
      guard = NoGuard,
      execute = \args -> do
        slot <- extractString "slot" args ??: "Missing 'slot' parameter"
        value <- extractString "value" args ??: "Missing 'value' parameter"

        -- Set the context variable in the Sentinel's context store
        let scalarValue = ScStr value
        lift $ setContextValue slot scalarValue []

        pure
          ToolOutput
            { observation = "Context established: " <> slot <> " = " <> value,
              producedFacts = []
            }
    }

--------------------------------------------------------------------------------
-- Guards
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

-- | Guard for refund tool with sophisticated proof-building.
--
-- Requires:
-- - User identity established
-- - Booking must exist (will be auto-fetched via tool binding)
-- - Refund eligibility through one of multiple paths
refundGuard :: Guard
refundGuard args = do
  bookingRef <- extractArg "bookingRef" args

  -- Require user identity
  _ <- queryPredicate "logged_in_user" []

  -- Booking must exist (auto-fetched via tool binding)
  _ <- queryPredicate "booking_passenger" [bookingRef]

  -- Booking source must be acceptable (not from travel agency or other airline)
  sourceProof <-
    oneOf
      [ do
          fact <- queryPredicate "booking_source" [bookingRef, ScStr "DirectAirCanada"]
          pure $ FactUsed fact,
        do
          fact <- queryPredicate "booking_source" [bookingRef, ScStr "GroupBooking"]
          pure $ FactUsed fact
      ]

  -- Check eligibility through one of the refund paths
  eligibilityProof <-
    oneOf
      [ do
          proof <- airlineAtFaultProof bookingRef
          pure $ RuleApplied "eligible_for_refund(full, airline_fault)" [proof],
        do
          proof <- eu261Proof bookingRef
          pure $ RuleApplied "eligible_for_refund(full, eu261)" [proof],
        do
          proof <- fareAllowsFullRefundProof bookingRef
          pure $ RuleApplied "eligible_for_refund(full, flexible_fare)" [proof],
        do
          proof <- fareAllowsPartialRefundProof bookingRef
          pure $ RuleApplied "eligible_for_refund(partial, standard_fare)" [proof],
        do
          proof <- bereavementProof bookingRef
          pure $ RuleApplied "eligible_for_refund(partial, bereavement)" [proof],
        do
          proof <- medicalBasicProof bookingRef
          pure $ RuleApplied "eligible_for_refund(voucher, medical_basic)" [proof]
      ]

  pure $ RuleApplied "refund_eligibility" [sourceProof, eligibilityProof]

--------------------------------------------------------------------------------
-- Proof Builders (from Rules.hs)
--------------------------------------------------------------------------------

-- | Get the second argument from a BaseFact.
--
-- Most predicates have the pattern @predicate(Key, Value)@ where the first
-- argument is the lookup key and the second is the extracted value.
getSecondArg :: BaseFact -> Scalar
getSecondArg (BaseFact _ factArgs) = case factArgs of
  (_ : v : _) -> v
  _ -> error "getSecondArg: fact has fewer than 2 arguments"

-- | Prove airline_at_fault(B): flight cancelled or delayed.
--
-- The airline is considered at fault if the flight is:
-- - Cancelled
-- - Delayed (any delay status qualifies)
airlineAtFaultProof :: Scalar -> SolverM Proof
airlineAtFaultProof booking =
  oneOf
    [ -- Flight is cancelled
      do
        -- Get the flight for this booking
        flightFact <- queryPredicate "booking_flight" [booking]
        let flightId = getSecondArg flightFact

        -- Check flight status
        statusFact <- queryPredicate "flight_status" [flightId]
        let status = getSecondArg statusFact

        -- Require status is "Cancelled"
        cancelledProof <-
          andAll
            [require (status == ScStr "Cancelled") "flight is cancelled"]

        pure $ RuleApplied "flight_cancelled" [FactUsed flightFact, FactUsed statusFact, cancelledProof],
      -- Flight is delayed
      do
        flightFact <- queryPredicate "booking_flight" [booking]
        let flightId = getSecondArg flightFact

        statusFact <- queryPredicate "flight_status" [flightId]
        let status = getSecondArg statusFact

        delayedProof <-
          andAll
            [require (status == ScStr "Delayed") "flight is delayed"]

        pure $ RuleApplied "flight_delayed" [FactUsed flightFact, FactUsed statusFact, delayedProof]
    ]

-- | Prove eu261_eligible(B): EU departure airport and significant delay/cancellation.
--
-- EU261 provides strong passenger rights for flights departing from EU airports
-- when there are significant delays or cancellations.
eu261Proof :: Scalar -> SolverM Proof
eu261Proof booking = do
  -- Get the flight for this booking
  flightFact <- queryPredicate "booking_flight" [booking]
  let flightId = getSecondArg flightFact

  -- Get flight origin
  originFact <- queryPredicate "flight_origin" [flightId]
  let origin = getSecondArg originFact

  -- Check flight status (must be cancelled or delayed)
  statusFact <- queryPredicate "flight_status" [flightId]
  let status = getSecondArg statusFact

  proofs <-
    andAll
      [ -- Origin must be an EU airport
        require (isEuAirport origin) "flight departs from EU airport",
        -- Flight must be cancelled or delayed
        require
          (status == ScStr "Cancelled" || status == ScStr "Delayed")
          "flight is cancelled or delayed"
      ]

  pure $ RuleApplied "eu261_eligible" [FactUsed flightFact, FactUsed originFact, FactUsed statusFact, proofs]
  where
    -- Simplified EU airport check
    isEuAirport :: Scalar -> Bool
    isEuAirport (ScStr code) =
      code
        `elem` [ "LHR", -- London Heathrow
                 "CDG", -- Paris Charles de Gaulle
                 "FRA", -- Frankfurt
                 "AMS", -- Amsterdam
                 "MAD", -- Madrid
                 "FCO", -- Rome Fiumicino
                 "MUC", -- Munich
                 "BCN", -- Barcelona
                 "ORY", -- Paris Orly
                 "DUB" -- Dublin
               ]
    isEuAirport _ = False

-- | Prove fare_allows_refund(B, full): fare class is Flexible/Business.
fareAllowsFullRefundProof :: Scalar -> SolverM Proof
fareAllowsFullRefundProof booking = do
  -- Get fare class for booking
  fareClassFact <- queryPredicate "booking_fare_class" [booking]
  let fareClass = getSecondArg fareClassFact

  -- Check the fare class is flexible (Business or First)
  let classMatches = case fareClass of
        ScStr "Business" -> True
        ScStr "First" -> True
        _ -> False

  proof <-
    andAll
      [require classMatches "fare class is flexible (Business/First)"]

  pure $ RuleApplied "fare_allows_refund(full)" [FactUsed fareClassFact, proof]

-- | Prove fare_allows_refund(B, partial): fare class is Standard/Economy.
fareAllowsPartialRefundProof :: Scalar -> SolverM Proof
fareAllowsPartialRefundProof booking = do
  -- Get fare class for booking
  fareClassFact <- queryPredicate "booking_fare_class" [booking]
  let fareClass = getSecondArg fareClassFact

  -- Check the fare class is standard (Economy or PremiumEconomy)
  let classMatches = case fareClass of
        ScStr "Economy" -> True
        ScStr "PremiumEconomy" -> True
        _ -> False

  proof <-
    andAll
      [require classMatches "fare class is standard (Economy/PremiumEconomy)"]

  pure $ RuleApplied "fare_allows_refund(partial)" [FactUsed fareClassFact, proof]

-- | Prove bereavement_eligible(B): user claims bereavement with documentation.
--
-- Bereavement exceptions require:
-- 1. User claims bereavement (askable)
-- 2. User confirms they will provide death certificate (askable)
bereavementProof :: Scalar -> SolverM Proof
bereavementProof _booking = do
  -- Get current user from context
  user <- contextVar "current_user"

  proofs <-
    andAll
      [ -- User claims bereavement
        askable "user_claims_bereavement" [user],
        -- User will provide documentation
        askable "user_confirms_documentation" [user, ScStr "death_certificate"]
      ]

  pure $ RuleApplied "bereavement_eligible" [proofs]

-- | Prove medical_basic_eligible(B): Basic Economy with medical emergency.
--
-- Basic Economy tickets are normally non-refundable, but a medical
-- emergency with documentation qualifies for a travel voucher.
medicalBasicProof :: Scalar -> SolverM Proof
medicalBasicProof booking = do
  -- Get current user from context
  user <- contextVar "current_user"

  -- Check ticket type is Basic Economy
  ticketTypeFact <- queryPredicate "booking_ticket_type" [booking]
  let ticketType = getSecondArg ticketTypeFact

  proofs <-
    andAll
      [ -- Must be EconomyBasic ticket
        require (ticketType == ScStr "EconomyBasic") "ticket is Basic Economy",
        -- User claims medical emergency
        askable "user_claims_medical_emergency" [user],
        -- User will provide documentation
        askable "user_confirms_documentation" [user, ScStr "medical_certificate"]
      ]

  pure $ RuleApplied "medical_basic_eligible" [FactUsed ticketTypeFact, proofs]

--------------------------------------------------------------------------------
-- Askable Declarations
--------------------------------------------------------------------------------

-- | Askable predicates for Air Canada refund policy.
airCanadaAskables :: AskableRegistry
airCanadaAskables =
  foldl'
    (flip declareAskable)
    emptyAskableRegistry
    [ -- User confirms they understand their booking will be cancelled
      AskableDecl
        { predicate = "user_confirms_cancellation",
          arity = 1,
          questionTemplate =
            "Do you understand that processing this refund will cancel your booking "
              <> "and the ticket will no longer be valid for travel?",
          evidenceType = UserStatement,
          description = "User confirms understanding that refund cancels booking"
        },
      -- User claims a medical emergency prevents them from traveling
      AskableDecl
        { predicate = "user_claims_medical_emergency",
          arity = 1,
          questionTemplate =
            "Are you requesting this refund due to a medical emergency "
              <> "that prevents you from traveling?",
          evidenceType = UserStatement,
          description = "User claims medical emergency"
        },
      -- User claims a bereavement situation
      AskableDecl
        { predicate = "user_claims_bereavement",
          arity = 1,
          questionTemplate =
            "Are you requesting this refund due to bereavement "
              <> "(death of passenger, immediate family member, or travel companion)?",
          evidenceType = UserStatement,
          description = "User claims bereavement circumstances"
        },
      -- User accepts voucher program terms and conditions
      AskableDecl
        { predicate = "user_accepts_voucher_terms",
          arity = 1,
          questionTemplate =
            "Do you accept the travel voucher terms? The voucher will be valid "
              <> "for 1 year from date of issue and can be used for any Air Canada flight.",
          evidenceType = ExplicitConfirmation,
          description = "User accepts voucher program T&C"
        },
      -- User confirms they will upload supporting documentation
      AskableDecl
        { predicate = "user_confirms_documentation",
          arity = 2,
          questionTemplate =
            "Can you provide a {1} to support your request? "
              <> "This documentation is required to process your refund.",
          evidenceType = DocumentUpload "{1}",
          description = "User confirms they will upload required documentation"
        }
    ]

--------------------------------------------------------------------------------
-- Context Declarations
--------------------------------------------------------------------------------

-- | Context variable declarations for Air Canada.
airCanadaContextDecls :: ContextDecls
airCanadaContextDecls =
  foldl'
    (flip declareContext)
    emptyContextDecls
    [ -- The currently logged-in user
      ContextDecl
        { name = "current_user",
          candidateQuery = Nothing, -- No query - must be seeded from session
          seedValue = Just (FromSession "user_id"),
          description = "The authenticated user making requests"
        },
      -- The booking the user is asking about
      ContextDecl
        { name = "booking_of_interest",
          candidateQuery = Just "user_bookings", -- Query to find candidates
          seedValue = Nothing, -- Not seeded - user must select
          description = "The booking the customer is asking about"
        },
      -- The flight associated with the booking of interest
      ContextDecl
        { name = "flight_of_interest",
          candidateQuery = Just "booking_flight", -- Derived from booking
          seedValue = Nothing, -- Derived, not directly seeded
          description = "The flight for the booking of interest"
        }
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
