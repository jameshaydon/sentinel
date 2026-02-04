-- | AirLogic Airlines tool definitions with embedded guards and execution logic.
--
-- Each tool is a self-contained definition including:
-- - Metadata for the LLM (name, description, params)
-- - Category (data vs action)
-- - Guard (precondition for execution)
-- - Execute (implementation returning observation + facts)
module Examples.AirLogic.Tools
  ( -- * Toolkit
    airLogicToolkit,
    airLogicSystemPrompt,

    -- * Data Tools
    getUserBookingsTool,
    getBookingTool,
    getFlightDetailsTool,
    getAirportInfoTool,
    getUserProfileTool,
    getRebookingPolicyTool,

    -- * Action Tools
    issueFullRefundTool,
    issuePartialRefundTool,
    issueVoucherTool,

    -- * Askables
    airLogicAskables,

    -- * Context
    airLogicContextDecls,
  )
where

import Data.Text qualified as T
import Examples.AirLogic.MockDB
  ( getAirportInfo,
    getBooking,
    getFlight,
    getRebookingPolicy,
    getUserBookings,
    getUserProfile,
  )
import Examples.AirLogic.ToolBindings (airLogicToolBindings)
import Examples.AirLogic.Types
import Pre
import Sentinel.Context (AskableSpec (..), ContextDecl (..), ContextDecls, SeedSpec (..), declareContext, emptyContextDecls, getContext)
import Sentinel.JSON (extractString)
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel (getContextStore, getDb)
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry, EvidenceType (..), declareAskable, emptyAskableRegistry)
import Sentinel.Solver.Combinators (SolverM, askable, contextVar, oneOf, queryAll, queryPredicate, require)
import Sentinel.Solver.Types (BaseFact (..), Proof (..), Scalar (..), ScalarType (..), factOutput, scalarToText)
import Sentinel.Tool (Guard, Query (..), Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))
import Sentinel.Toolkit (Toolkit (..))

--------------------------------------------------------------------------------
-- Toolkit
--------------------------------------------------------------------------------

-- | The AirLogic Airlines toolkit with all tools and system prompt.
airLogicToolkit :: Toolkit AirLogicDB
airLogicToolkit =
  Toolkit
    { tools =
        [ getUserBookingsTool,
          getBookingTool,
          getFlightDetailsTool,
          getAirportInfoTool,
          getUserProfileTool,
          getRebookingPolicyTool,
          issueFullRefundTool,
          issuePartialRefundTool,
          issueVoucherTool
        ],
      queries = [refundEligibilityQuery],
      systemPrompt = airLogicSystemPrompt,
      toolBindings = airLogicToolBindings,
      askables = airLogicAskables,
      contextDecls = airLogicContextDecls
    }

--------------------------------------------------------------------------------
-- System Prompt
--------------------------------------------------------------------------------

-- | System prompt for the AirLogic Airlines agent.
airLogicSystemPrompt :: Text
airLogicSystemPrompt =
  T.unlines
    [ "You are a helpful customer service agent for AirLogic Airlines.",
      "You help customers with flight information, booking inquiries, and refund requests.",
      "",
      "CRITICAL RULES:",
      "- You MUST use the QueryRefundEligibility tool before making ANY statements about",
      "  whether a customer is or is not eligible for a refund, voucher, or compensation.",
      "- NEVER determine refund eligibility yourself from raw flight or booking data.",
      "  The eligibility rules are complex and encoded in the system's solver. Only the",
      "  QueryRefundEligibility result is authoritative.",
      "- After calling QueryRefundEligibility, faithfully report what the solver found:",
      "  which paths succeeded, which are blocked (and what would unblock them), and",
      "  which failed.",
      "- You may freely use data tools (GetFlightDetails, GetBooking, etc.) to answer",
      "  general informational questions that are not about refund eligibility."
    ]

--------------------------------------------------------------------------------
-- Data Tools
--------------------------------------------------------------------------------

-- | Get user bookings tool.
getUserBookingsTool :: Tool AirLogicDB
getUserBookingsTool =
  Tool
    { name = "GetUserBookings",
      description = "Get all bookings for a user. Use this to find what bookings a customer has.",
      params =
        Schema.objectSchema
          [("userId", Schema.stringProp "The user's unique identifier")]
          ["userId"],
      category = DataTool,
      guard = NoGuard,
      execute = \args -> do
        userId <- extractString "userId" args ??: "Missing or invalid 'userId' parameter"
        db <- lift getDb
        let bookings = getUserBookings userId db
        if null bookings
          then throwError $ "No bookings found for user: " <> userId
          else
            pure
              ToolOutput
                { observation =
                    renderDocPlain
                      $ vsep
                        [ "Found" <+> pretty (length bookings) <+> "booking(s):",
                          mempty,
                          vsep (punctuate (line <> "---" <> line) (fmap disp bookings))
                        ],
                  producedFacts = concatMap bookingToFacts' bookings,
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
    }
  where
    bookingToFacts' b =
      [BaseFact "user_bookings" [ScStr b.userId, ScStr b.bookingId]]

-- | Get booking details tool.
getBookingTool :: Tool AirLogicDB
getBookingTool =
  Tool
    { name = "GetBooking",
      description = "Get details of a specific booking by ID. Returns fare class, flight, amount, and hours until departure.",
      params =
        Schema.objectSchema
          [("bookingId", Schema.stringProp "The booking reference (e.g., BK-2847)")]
          ["bookingId"],
      category = DataTool,
      guard = NoGuard,
      execute = \args -> do
        bookingId <- extractString "bookingId" args ??: "Missing or invalid 'bookingId' parameter"
        db <- lift getDb
        case getBooking bookingId db of
          Just booking ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp booking),
                  producedFacts = bookingToFacts booking,
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
          Nothing -> throwError $ "No booking found with ID: " <> bookingId
    }

-- | Get flight details tool.
getFlightDetailsTool :: Tool AirLogicDB
getFlightDetailsTool =
  Tool
    { name = "GetFlightDetails",
      description = "Get details of a flight including status, delay, and airports.",
      params =
        Schema.objectSchema
          [("flightId", Schema.stringProp "The flight number (e.g., AL-445)")]
          ["flightId"],
      category = DataTool,
      guard = NoGuard,
      execute = \args -> do
        flightId <- extractString "flightId" args ??: "Missing or invalid 'flightId' parameter"
        db <- lift getDb
        case getFlight flightId db of
          Just flight ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp flight),
                  producedFacts = flightToFacts flight,
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
          Nothing -> throwError $ "No flight found with ID: " <> flightId
    }

-- | Get airport info tool.
getAirportInfoTool :: Tool AirLogicDB
getAirportInfoTool =
  Tool
    { name = "GetAirportInfo",
      description = "Get information about an airport, including whether it's in the EU (for EU261 eligibility).",
      params =
        Schema.objectSchema
          [("code", Schema.stringProp "The airport code (e.g., LHR, JFK)")]
          ["code"],
      category = DataTool,
      guard = NoGuard,
      execute = \args -> do
        code <- extractString "code" args ??: "Missing or invalid 'code' parameter"
        db <- lift getDb
        case getAirportInfo code db of
          Just airport ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp airport),
                  producedFacts = airportToFacts airport,
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
          Nothing -> throwError $ "No airport found with code: " <> code
    }

-- | Get user profile tool.
getUserProfileTool :: Tool AirLogicDB
getUserProfileTool =
  Tool
    { name = "GetUserProfile",
      description = "Get a user's profile including loyalty tier.",
      params =
        Schema.objectSchema
          [("userId", Schema.stringProp "The user's unique identifier")]
          ["userId"],
      category = DataTool,
      guard = NoGuard,
      execute = \args -> do
        userId <- extractString "userId" args ??: "Missing or invalid 'userId' parameter"
        db <- lift getDb
        case getUserProfile userId db of
          Just user ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp user),
                  producedFacts = userToFacts user,
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
          Nothing -> throwError $ "No user found with ID: " <> userId
    }

-- | Get rebooking policy tool.
getRebookingPolicyTool :: Tool AirLogicDB
getRebookingPolicyTool =
  Tool
    { name = "GetRebookingPolicy",
      description = "Get the rebooking policy for a fare class, including whether rebooking is allowed and the change fee.",
      params =
        Schema.objectSchema
          [("fareClass", Schema.enumProp ["Flexible", "Standard", "Basic"] "The fare class to check")]
          ["fareClass"],
      category = DataTool,
      guard = NoGuard,
      execute = \args -> do
        fareClassStr <- extractString "fareClass" args ??: "Missing or invalid 'fareClass' parameter"
        fareClass <- parseFareClass fareClassStr ??: "Invalid fare class: " <> fareClassStr
        db <- lift getDb
        case getRebookingPolicy fareClass db of
          Just policy ->
            pure
              ToolOutput
                { observation = renderDocPlain (disp policy),
                  producedFacts = rebookingPolicyToFacts policy,
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
          Nothing -> throwError $ "No rebooking policy found for fare class: " <> fareClassStr
    }

--------------------------------------------------------------------------------
-- Action Tools
--------------------------------------------------------------------------------

-- | Issue full refund tool.
issueFullRefundTool :: Tool AirLogicDB
issueFullRefundTool =
  Tool
    { name = "IssueFullRefund",
      description =
        "Process a FULL refund (no fees deducted) for the booking of interest. "
          <> "CHECK THIS FIRST when a customer requests a refund, as it provides the best outcome. "
          <> "Eligibility: (1) Airline fault - flight cancelled or delayed >3 hours, "
          <> "(2) EU261 protection - EU departure and delay >5 hours, "
          <> "(3) Flexible fare class with >24 hours notice. "
          <> "Requires customer confirmation that they understand the booking will be cancelled.",
      params = Schema.objectSchema [] [],
      category = ActionTool,
      guard = SolverGuardT "full_refund_eligibility" fullRefundGuard,
      execute = \_ -> do
        ctxStore <- lift getContextStore
        bookingId <-
          getContext "booking_of_interest" ctxStore
            & fmap scalarToText
            ??: "booking_of_interest context not set"
        db <- lift getDb
        case getBooking bookingId db of
          Nothing -> throwError $ "Booking not found: " <> bookingId
          Just booking -> do
            let amount = formatCents booking.totalAmountCents
            pure
              ToolOutput
                { observation =
                    "Full refund of "
                      <> amount
                      <> " approved for booking "
                      <> bookingId
                      <> ". Amount will be credited to the original payment method within 3-5 business days.",
                  producedFacts = [BaseFact "refund_issued" [ScStr bookingId, ScStr "full", ScNum (fromIntegral booking.totalAmountCents)]],
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
    }

-- | Issue partial refund tool.
issuePartialRefundTool :: Tool AirLogicDB
issuePartialRefundTool =
  Tool
    { name = "IssuePartialRefund",
      description =
        "Process a PARTIAL refund ($150 fee deducted) for the booking of interest. "
          <> "Only use this if full refund is not available - this is for voluntary cancellations. "
          <> "Eligibility: (1) Standard fare class with >72 hours until departure, "
          <> "(2) Bereavement with documentation. "
          <> "Requires customer confirmation that they understand the booking will be cancelled.",
      params = Schema.objectSchema [] [],
      category = ActionTool,
      guard = SolverGuardT "partial_refund_eligibility" partialRefundGuard,
      execute = \_ -> do
        ctxStore <- lift getContextStore
        bookingId <-
          getContext "booking_of_interest" ctxStore
            & fmap scalarToText
            ??: "booking_of_interest context not set"
        db <- lift getDb
        case getBooking bookingId db of
          Nothing -> throwError $ "Booking not found: " <> bookingId
          Just booking -> do
            let fee = 15000 :: Int
            let refundAmount = max 0 (booking.totalAmountCents - fee)
            pure
              ToolOutput
                { observation =
                    "Partial refund of "
                      <> formatCents refundAmount
                      <> " approved for booking "
                      <> bookingId
                      <> " (after $150 cancellation fee). "
                      <> "Amount will be credited to the original payment method within 3-5 business days.",
                  producedFacts = [BaseFact "refund_issued" [ScStr bookingId, ScStr "partial", ScNum (fromIntegral refundAmount)]],
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
    }

-- | Issue voucher tool.
issueVoucherTool :: Tool AirLogicDB
issueVoucherTool =
  Tool
    { name = "IssueVoucher",
      description =
        "Issue a travel voucher for the booking of interest. "
          <> "Available for Basic Economy with medical emergency documentation. "
          <> "Voucher is valid for 1 year and non-transferable. "
          <> "Requires customer acceptance of voucher terms and conditions.",
      params = Schema.objectSchema [] [],
      category = ActionTool,
      guard = SolverGuardT "voucher_eligibility" voucherGuard,
      execute = \_ -> do
        ctxStore <- lift getContextStore
        bookingId <-
          getContext "booking_of_interest" ctxStore
            & fmap scalarToText
            ??: "booking_of_interest context not set"
        db <- lift getDb
        case getBooking bookingId db of
          Nothing -> throwError $ "Booking not found: " <> bookingId
          Just booking -> do
            let amount = formatCents booking.totalAmountCents
            pure
              ToolOutput
                { observation =
                    "Travel voucher of "
                      <> amount
                      <> " issued for booking "
                      <> bookingId
                      <> ". Voucher is valid for 12 months and can be used for any AirLogic Airlines booking. "
                      <> "Voucher code: VCH-"
                      <> bookingId
                      <> "-2025",
                  producedFacts = [BaseFact "voucher_issued" [ScStr bookingId, ScNum (fromIntegral booking.totalAmountCents)]],
                  triggerSideSession = Nothing,
                  solverOutcome = Nothing
                }
    }

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Query for checking refund eligibility across all paths.
--
-- This is a first-class query: the solver evaluates all refund eligibility
-- paths (full, partial, voucher) and reports proofs/blocks directly to the LLM.
-- Uses the same eligibility logic as the guards on the action tools but gets
-- the booking from context.
refundEligibilityQuery :: Query
refundEligibilityQuery =
  Query
    { name = "QueryRefundEligibility",
      description =
        "Check if the current booking is eligible for a refund. "
          <> "Returns available refund options (full, partial, voucher) with reasons. "
          <> "May indicate that context needs to be established (which booking?) "
          <> "or that user confirmation is needed. "
          <> "Call this repeatedly after answering blocked questions to discover more paths.",
      params = Schema.emptyObjectSchema,
      goal = \_args -> do
        (bookingId, user) <- getBookingOfInterest

        -- Check eligibility through all refund paths
        eligibilityProof <-
          oneOf
            [ do
                proof <- airlineFaultProof bookingId
                pure $ RuleApplied "eligible_for_refund" [ScStr "full", ScStr "airline_fault"] [proof],
              do
                proof <- eu261Proof bookingId
                pure $ RuleApplied "eligible_for_refund" [ScStr "full", ScStr "eu261"] [proof],
              do
                proof <- flexibleFareProof bookingId
                pure $ RuleApplied "eligible_for_refund" [ScStr "full", ScStr "flexible_fare"] [proof],
              do
                proof <- standardFareProof bookingId
                pure $ RuleApplied "eligible_for_refund" [ScStr "partial", ScStr "standard_fare"] [proof],
              do
                proof <- bereavementProof user
                pure $ RuleApplied "eligible_for_refund" [ScStr "partial", ScStr "bereavement"] [proof],
              do
                proof <- voucherEligibilityProof bookingId user
                pure $ RuleApplied "eligible_for_refund" [ScStr "voucher", ScStr "medical_basic"] [proof]
            ]

        pure $ RuleApplied "refund_eligibility" [] [ContextBound "current_user" user, eligibilityProof]
    }

-- | Prove voucher eligibility: Basic Economy with medical documentation.
voucherEligibilityProof :: Scalar -> Scalar -> SolverM Proof
voucherEligibilityProof bookingId user = do
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact & factOutput
  fareCheck <- require (fareClass == ScStr "Basic") "fare_class == Basic"
  let fareProof = RuleApplied "basic_fare_class" [] [FactUsed fareClassFact, fareCheck]

  medicalProof <- askable "user_claims_medical_emergency" [user]
  docProof <- askable "user_confirms_upload_documentation" [user, ScStr "medical_certificate"]
  termsProof <- askable "user_accepts_voucher_terms" [user]

  pure $ RuleApplied "voucher_eligible" [] [fareProof, medicalProof, docProof, termsProof]

--------------------------------------------------------------------------------
-- Shared Helpers
--------------------------------------------------------------------------------

-- | Get the booking of interest, with dynamically computed candidate options.
--
-- First resolves the current user, then queries the fact store for the user's
-- bookings (invoking the data tool if needed). The booking IDs are passed as
-- candidates to 'contextVar', so the frontend can render them as clickable buttons.
getBookingOfInterest :: SolverM (Scalar, Scalar)
getBookingOfInterest = do
  user <- contextVar "current_user" Nothing
  bookingFacts <- queryAll "user_bookings" [user]
  let bookingIds = map factOutput bookingFacts
  bookingId <- contextVar "booking_of_interest" (Just bookingIds)
  pure (bookingId, user)

--------------------------------------------------------------------------------
-- Guards
--------------------------------------------------------------------------------

-- | Extract a Double from a Scalar, defaulting to 0 if not a number.
getScalarNum :: Scalar -> Double
getScalarNum = \case
  ScNum n -> n
  _ -> 0

-- | Guard for full refund: eligible via airline_fault, eu261, or flexible_fare.
-- Also requires user confirmation of cancellation understanding.
fullRefundGuard :: Guard
fullRefundGuard _args = do
  (bookingId, user) <- getBookingOfInterest

  -- Check eligibility through one of the full refund paths
  eligibilityProof <-
    oneOf
      [ do
          proof <- airlineFaultProof bookingId
          pure $ RuleApplied "eligible_for_refund" [ScStr "full", ScStr "airline_fault"] [proof],
        do
          proof <- eu261Proof bookingId
          pure $ RuleApplied "eligible_for_refund" [ScStr "full", ScStr "eu261"] [proof],
        do
          proof <- flexibleFareProof bookingId
          pure $ RuleApplied "eligible_for_refund" [ScStr "full", ScStr "flexible_fare"] [proof]
      ]

  -- Require user confirmation before proceeding
  confirmProof <- askable "user_confirms_cancellation_understanding" [user]

  pure $ RuleApplied "full_refund_eligibility" [] [eligibilityProof, confirmProof]

-- | Prove airline_at_fault(B): flight cancelled or delay > 180 minutes (3 hours).
airlineFaultProof :: Scalar -> SolverM Proof
airlineFaultProof bookingId = do
  -- Get the flight for this booking
  flightFact <- queryPredicate "booking_flight" [bookingId]
  let flightId = flightFact & factOutput

  -- Check if airline is at fault (via cancellation or delay)
  faultProof <-
    oneOf
      [ do
          -- Path 1: Flight cancelled
          statusFact <- queryPredicate "flight_status" [flightId]
          let status = statusFact & factOutput
          proof <- require (status == ScStr "Cancelled") "status == Cancelled"
          pure $ RuleApplied "flight_cancelled" [] [FactUsed statusFact, proof],
        do
          -- Path 2: Delay > 180 minutes
          delayFact <- queryPredicate "flight_delay_minutes" [flightId]
          let mins = getScalarNum (delayFact & factOutput)
          proof <- require (mins > 180) "delay_minutes > 180"
          pure $ RuleApplied "significant_delay" [ScStr ">3h"] [FactUsed delayFact, proof]
      ]

  pure $ RuleApplied "airline_at_fault" [] [FactUsed flightFact, faultProof]

-- | Prove eu261_eligible(B): EU departure airport and delay > 300 minutes (5 hours).
eu261Proof :: Scalar -> SolverM Proof
eu261Proof bookingId = do
  -- Get the flight for this booking
  flightFact <- queryPredicate "booking_flight" [bookingId]
  let flightId = flightFact & factOutput

  -- Get departure airport and prove it's in EU
  airportFact <- queryPredicate "flight_departure_airport" [flightId]
  let airportCode = airportFact & factOutput
  euFact <- queryPredicate "is_eu_airport" [airportCode]
  let euAirportProof = RuleApplied "eu_departure" [] [FactUsed airportFact, FactUsed euFact]

  -- Check delay > 300 minutes
  delayFact <- queryPredicate "flight_delay_minutes" [flightId]
  let mins = getScalarNum (delayFact & factOutput)
  delayCheck <- require (mins > 300) "delay_minutes > 300"
  let delayProof = RuleApplied "eu261_delay" [ScStr ">5h"] [FactUsed delayFact, delayCheck]

  pure $ RuleApplied "eu261_eligible" [] [FactUsed flightFact, euAirportProof, delayProof]

-- | Prove fare_allows_refund(B, full): fare class is Flexible and > 24 hours until departure.
flexibleFareProof :: Scalar -> SolverM Proof
flexibleFareProof bookingId = do
  -- Check fare class is flexible
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact & factOutput
  fareCheck <- require (fareClass == ScStr "Flexible") "fare_class == Flexible"
  let fareProof = RuleApplied "flexible_fare_class" [] [FactUsed fareClassFact, fareCheck]

  -- Check hours until departure > 24
  hoursFact <- queryPredicate "hours_until_departure" [bookingId]
  let hours = getScalarNum (hoursFact & factOutput)
  hoursCheck <- require (hours > 24) "hours_until_departure > 24"
  let hoursProof = RuleApplied "advance_notice" [ScStr ">24h"] [FactUsed hoursFact, hoursCheck]

  pure $ RuleApplied "fare_allows_refund" [ScStr "full"] [fareProof, hoursProof]

-- | Guard for partial refund: eligible via standard_fare or bereavement.
-- Also requires user confirmation of cancellation understanding.
partialRefundGuard :: Guard
partialRefundGuard _args = do
  (bookingId, user) <- getBookingOfInterest

  -- Check eligibility through one of the partial refund paths
  eligibilityProof <-
    oneOf
      [ do
          proof <- standardFareProof bookingId
          pure $ RuleApplied "eligible_for_refund" [ScStr "partial", ScStr "standard_fare"] [proof],
        do
          proof <- bereavementProof user
          pure $ RuleApplied "eligible_for_refund" [ScStr "partial", ScStr "bereavement"] [proof]
      ]

  -- Require user confirmation before proceeding
  confirmProof <- askable "user_confirms_cancellation_understanding" [user]

  pure $ RuleApplied "partial_refund_eligibility" [] [eligibilityProof, confirmProof]

-- | Prove fare_allows_refund(B, partial): fare class is Standard and > 72 hours until departure.
standardFareProof :: Scalar -> SolverM Proof
standardFareProof bookingId = do
  -- Check fare class is standard
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact & factOutput
  fareCheck <- require (fareClass == ScStr "Standard") "fare_class == Standard"
  let fareProof = RuleApplied "standard_fare_class" [] [FactUsed fareClassFact, fareCheck]

  -- Check hours until departure > 72
  hoursFact <- queryPredicate "hours_until_departure" [bookingId]
  let hours = getScalarNum (hoursFact & factOutput)
  hoursCheck <- require (hours > 72) "hours_until_departure > 72"
  let hoursProof = RuleApplied "advance_notice" [ScStr ">72h"] [FactUsed hoursFact, hoursCheck]

  pure $ RuleApplied "fare_allows_refund" [ScStr "partial"] [fareProof, hoursProof]

-- | Prove bereavement_eligible(U): user claims bereavement and confirms documentation.
bereavementProof :: Scalar -> SolverM Proof
bereavementProof user = do
  claimProof <- askable "user_claims_bereavement" [user]
  docProof <- askable "user_confirms_upload_documentation" [user, ScStr "death_certificate"]
  pure $ RuleApplied "bereavement_eligible" [] [claimProof, docProof]

-- | Guard for voucher: eligible for Basic Economy with medical documentation.
voucherGuard :: Guard
voucherGuard _args = do
  (bookingId, user) <- getBookingOfInterest

  -- Check fare class is Basic
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact & factOutput
  fareCheck <- require (fareClass == ScStr "Basic") "fare_class == Basic"
  let fareProof = RuleApplied "basic_fare_class" [] [FactUsed fareClassFact, fareCheck]

  -- User must claim medical emergency
  medicalProof <- askable "user_claims_medical_emergency" [user]

  -- User must confirm they will upload medical documentation
  docProof <- askable "user_confirms_upload_documentation" [user, ScStr "medical_certificate"]

  -- User must accept voucher terms and conditions
  termsProof <- askable "user_accepts_voucher_terms" [user]

  let eligibilityProof = RuleApplied "fare_allows_refund" [ScStr "voucher"] [fareProof, medicalProof, docProof, termsProof]

  pure $ RuleApplied "voucher_eligibility" [] [eligibilityProof]

--------------------------------------------------------------------------------
-- Askable Declarations
--------------------------------------------------------------------------------

-- | Askable predicates for AirLogic Airlines.
airLogicAskables :: AskableRegistry
airLogicAskables =
  foldl'
    (flip declareAskable)
    emptyAskableRegistry
    [ AskableDecl
        { predicate = "user_confirms_cancellation_understanding",
          argumentTypes = [TextType], -- [user_id]
          questionTemplate = "Do you understand that if we process this refund, your booking will be cancelled and your ticket will become invalid?",
          evidenceType = ExplicitConfirmation,
          description = "User confirms they understand the booking will be cancelled"
        },
      AskableDecl
        { predicate = "user_claims_medical_emergency",
          argumentTypes = [TextType], -- [user_id]
          questionTemplate = "Are you requesting this refund due to a medical emergency that prevents you from traveling?",
          evidenceType = UserStatement,
          description = "User claims medical emergency"
        },
      AskableDecl
        { predicate = "user_claims_bereavement",
          argumentTypes = [TextType], -- [user_id]
          questionTemplate = "Are you requesting this refund due to bereavement (death of an immediate family member)?",
          evidenceType = UserStatement,
          description = "User claims bereavement circumstances"
        },
      AskableDecl
        { predicate = "user_accepts_voucher_terms",
          argumentTypes = [TextType], -- [user_id]
          questionTemplate = "Do you accept the terms and conditions of the travel voucher program (voucher valid for 1 year, non-transferable)?",
          evidenceType = ExplicitConfirmation,
          description = "User accepts voucher T&C"
        },
      AskableDecl
        { predicate = "user_confirms_upload_documentation",
          argumentTypes = [TextType, TextType], -- [user_id, document_type]
          questionTemplate = "Can you provide a {1} to support your request?",
          evidenceType = DocumentUpload "documentation",
          description = "User confirms they will upload required documentation"
        }
    ]

--------------------------------------------------------------------------------
-- Context Declarations
--------------------------------------------------------------------------------

-- | Context variable declarations for AirLogic Airlines.
airLogicContextDecls :: ContextDecls
airLogicContextDecls =
  foldl'
    (flip declareContext)
    emptyContextDecls
    [ ContextDecl
        { name = "current_user",
          valueType = TextType,
          seedValue = Just (FromSession "user_id"),
          askable = Nothing, -- Cannot be asked, only pre-seeded
          description = "The authenticated user's ID"
        },
      ContextDecl
        { name = "booking_of_interest",
          valueType = TextType,
          seedValue = Nothing,
          askable =
            Just
              AskableSpec
                { questionTemplate = "Which booking are you asking about?",
                  candidates = [] -- Candidates provided dynamically by LLM
                },
          description = "The booking the customer is asking about"
        }
    ]

--------------------------------------------------------------------------------
-- Fact Production
--------------------------------------------------------------------------------

-- | Convert a booking to its constituent BaseFacts.
bookingToFacts :: Booking -> [BaseFact]
bookingToFacts b =
  [ BaseFact "booking_fare_class" [ScStr b.bookingId, ScStr (fareClassToText b.fareClass)],
    BaseFact "booking_amount" [ScStr b.bookingId, ScNum (fromIntegral b.totalAmountCents)],
    BaseFact "booking_flight" [ScStr b.bookingId, ScStr b.flightId],
    BaseFact "booking_user" [ScStr b.bookingId, ScStr b.userId],
    BaseFact "hours_until_departure" [ScStr b.bookingId, ScNum (fromIntegral b.hoursUntilDeparture)]
  ]

-- | Convert a flight to its constituent BaseFacts.
flightToFacts :: Flight -> [BaseFact]
flightToFacts f =
  [ BaseFact "flight_status" [ScStr f.flightId, ScStr (flightStatusToText f.status)],
    BaseFact "flight_delay_minutes" [ScStr f.flightId, ScNum (fromIntegral f.delayMinutes)],
    BaseFact "flight_departure_airport" [ScStr f.flightId, ScStr f.departureAirport],
    BaseFact "flight_arrival_airport" [ScStr f.flightId, ScStr f.arrivalAirport]
  ]

-- | Convert an airport to its constituent BaseFacts.
airportToFacts :: Airport -> [BaseFact]
airportToFacts a =
  if a.isEU
    then [BaseFact "is_eu_airport" [ScStr a.code]]
    else []

-- | Convert a user to its constituent BaseFacts.
userToFacts :: User -> [BaseFact]
userToFacts u =
  [BaseFact "user_loyalty_tier" [ScStr u.oduserId, ScStr (loyaltyTierToText u.loyaltyTier)]]

-- | Convert a rebooking policy to its constituent BaseFacts.
rebookingPolicyToFacts :: RebookingPolicy -> [BaseFact]
rebookingPolicyToFacts p =
  let fc = fareClassToText p.policyFareClass
   in [ BaseFact "rebooking_fee" [ScStr fc, ScNum (fromIntegral p.changeFee)]
      ]
        ++ [BaseFact "rebooking_allowed" [ScStr fc] | p.rebookingAllowed]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Parse a fare class from text.
parseFareClass :: Text -> Maybe FareClass
parseFareClass = \case
  "Flexible" -> Just Flexible
  "Standard" -> Just Standard
  "Basic" -> Just Basic
  _ -> Nothing

-- | Convert fare class to text.
fareClassToText :: FareClass -> Text
fareClassToText = \case
  Flexible -> "Flexible"
  Standard -> "Standard"
  Basic -> "Basic"

-- | Convert flight status to text.
flightStatusToText :: FlightStatus -> Text
flightStatusToText = \case
  OnTime -> "OnTime"
  Delayed -> "Delayed"
  Cancelled -> "Cancelled"

-- | Convert loyalty tier to text.
loyaltyTierToText :: LoyaltyTier -> Text
loyaltyTierToText = \case
  Bronze -> "Bronze"
  Silver -> "Silver"
  Gold -> "Gold"
  Platinum -> "Platinum"
