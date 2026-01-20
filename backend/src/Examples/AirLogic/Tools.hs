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
import Sentinel.Context (ContextDecl (..), ContextDecls, SeedSpec (..), declareContext, emptyContextDecls)
import Sentinel.JSON (extractString)
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel (getDb)
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry, EvidenceType (..), declareAskable, emptyAskableRegistry)
import Sentinel.Solver.Combinators (SolverM, extractArg, oneOf, queryPredicate, require)
import Sentinel.Solver.Types (BaseFact (..), Proof (..), Scalar (..))
import Sentinel.Tool (Guard, Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))
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
      "REFUND POLICY SUMMARY:",
      "1. Airline-Caused Disruptions: Full refund if flight cancelled or delay > 3 hours",
      "2. EU261 Protection: Full refund if EU departure and delay > 5 hours",
      "3. Flexible Fare: Full refund if requested > 24 hours before departure",
      "4. Standard Fare: Partial refund (minus $150 fee) if requested > 72 hours before departure",
      "5. Basic Economy: No cash refund, but credit voucher with medical documentation",
      "6. Bereavement: Partial refund with death certificate of immediate family member",
      "",
      "IMPORTANT INSTRUCTIONS:",
      "1. Always verify the customer's identity before accessing their bookings",
      "2. Check refund eligibility through all possible paths before processing",
      "3. Clearly explain why a refund is approved or denied",
      "4. Offer alternative options (vouchers, rebooking) when refund is not available",
      "5. Ask for confirmation before processing any refund",
      "",
      "Always be polite, professional, and empathetic with customers."
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
                  producedFacts = concatMap bookingToFacts' bookings
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
                  producedFacts = bookingToFacts booking
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
                  producedFacts = flightToFacts flight
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
                  producedFacts = airportToFacts airport
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
                  producedFacts = userToFacts user
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
                  producedFacts = rebookingPolicyToFacts policy
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
        "Process a full refund for an eligible booking. "
          <> "Customer must be eligible via airline fault, EU261, or flexible fare. "
          <> "Requires customer confirmation that they understand the booking will be cancelled.",
      params =
        Schema.objectSchema
          [("bookingId", Schema.stringProp "The booking reference to refund")]
          ["bookingId"],
      category = ActionTool,
      guard = SolverGuardT "full_refund_eligibility" fullRefundGuard,
      execute = \args -> do
        bookingId <- extractString "bookingId" args ??: "Missing or invalid 'bookingId' parameter"
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
                  producedFacts = [BaseFact "refund_issued" [ScStr bookingId, ScStr "full", ScNum (fromIntegral booking.totalAmountCents)]]
                }
    }

-- | Issue partial refund tool.
issuePartialRefundTool :: Tool AirLogicDB
issuePartialRefundTool =
  Tool
    { name = "IssuePartialRefund",
      description =
        "Process a partial refund for an eligible booking. "
          <> "Customer must be eligible via standard fare (> 72 hours advance) or bereavement. "
          <> "A $150 cancellation fee will be deducted. "
          <> "Requires customer confirmation that they understand the booking will be cancelled.",
      params =
        Schema.objectSchema
          [("bookingId", Schema.stringProp "The booking reference to refund")]
          ["bookingId"],
      category = ActionTool,
      guard = SolverGuardT "partial_refund_eligibility" partialRefundGuard,
      execute = \args -> do
        bookingId <- extractString "bookingId" args ??: "Missing or invalid 'bookingId' parameter"
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
                  producedFacts = [BaseFact "refund_issued" [ScStr bookingId, ScStr "partial", ScNum (fromIntegral refundAmount)]]
                }
    }

-- | Issue voucher tool.
issueVoucherTool :: Tool AirLogicDB
issueVoucherTool =
  Tool
    { name = "IssueVoucher",
      description =
        "Issue a travel voucher for an eligible booking. "
          <> "Available for Basic Economy with medical emergency documentation. "
          <> "Voucher is valid for 1 year and non-transferable. "
          <> "Requires customer acceptance of voucher terms and conditions.",
      params =
        Schema.objectSchema
          [("bookingId", Schema.stringProp "The booking reference for the voucher")]
          ["bookingId"],
      category = ActionTool,
      guard = SolverGuardT "voucher_eligibility" voucherGuard,
      execute = \args -> do
        bookingId <- extractString "bookingId" args ??: "Missing or invalid 'bookingId' parameter"
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
                  producedFacts = [BaseFact "voucher_issued" [ScStr bookingId, ScNum (fromIntegral booking.totalAmountCents)]]
                }
    }

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
fullRefundGuard args = do
  bookingId <- extractArg "bookingId" args

  -- Check eligibility through one of the full refund paths
  eligibilityProof <-
    oneOf
      [ do
          proof <- airlineFaultProof bookingId
          pure $ RuleApplied "eligible_for_refund(full, airline_fault)" [proof],
        do
          proof <- eu261Proof bookingId
          pure $ RuleApplied "eligible_for_refund(full, eu261)" [proof],
        do
          proof <- flexibleFareProof bookingId
          pure $ RuleApplied "eligible_for_refund(full, flexible_fare)" [proof]
      ]

  pure $ RuleApplied "full_refund_eligibility" [eligibilityProof]

-- | Prove airline_at_fault(B): flight cancelled or delay > 180 minutes (3 hours).
airlineFaultProof :: Scalar -> SolverM Proof
airlineFaultProof bookingId = do
  -- Get the flight for this booking
  flightFact <- queryPredicate "booking_flight" [bookingId]
  let flightId = flightFact.arguments !! 1

  -- Check if airline is at fault (via cancellation or delay)
  faultProof <-
    oneOf
      [ do
          -- Path 1: Flight cancelled
          statusFact <- queryPredicate "flight_status" [flightId]
          let status = statusFact.arguments !! 1
          proof <- require (status == ScStr "Cancelled") "status == Cancelled"
          pure $ RuleApplied "flight_cancelled" [FactUsed statusFact, proof],
        do
          -- Path 2: Delay > 180 minutes
          delayFact <- queryPredicate "flight_delay_minutes" [flightId]
          let mins = getScalarNum (delayFact.arguments !! 1)
          proof <- require (mins > 180) "delay_minutes > 180"
          pure $ RuleApplied "significant_delay(>3h)" [FactUsed delayFact, proof]
      ]

  pure $ RuleApplied "airline_at_fault" [FactUsed flightFact, faultProof]

-- | Prove eu261_eligible(B): EU departure airport and delay > 300 minutes (5 hours).
eu261Proof :: Scalar -> SolverM Proof
eu261Proof bookingId = do
  -- Get the flight for this booking
  flightFact <- queryPredicate "booking_flight" [bookingId]
  let flightId = flightFact.arguments !! 1

  -- Get departure airport and prove it's in EU
  airportFact <- queryPredicate "flight_departure_airport" [flightId]
  let airportCode = airportFact.arguments !! 1
  euFact <- queryPredicate "is_eu_airport" [airportCode]
  let euAirportProof = RuleApplied "eu_departure" [FactUsed airportFact, FactUsed euFact]

  -- Check delay > 300 minutes
  delayFact <- queryPredicate "flight_delay_minutes" [flightId]
  let mins = getScalarNum (delayFact.arguments !! 1)
  delayCheck <- require (mins > 300) "delay_minutes > 300"
  let delayProof = RuleApplied "eu261_delay(>5h)" [FactUsed delayFact, delayCheck]

  pure $ RuleApplied "eu261_eligible" [FactUsed flightFact, euAirportProof, delayProof]

-- | Prove fare_allows_refund(B, full): fare class is Flexible and > 24 hours until departure.
flexibleFareProof :: Scalar -> SolverM Proof
flexibleFareProof bookingId = do
  -- Check fare class is flexible
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact.arguments !! 1
  fareCheck <- require (fareClass == ScStr "Flexible") "fare_class == Flexible"
  let fareProof = RuleApplied "flexible_fare_class" [FactUsed fareClassFact, fareCheck]

  -- Check hours until departure > 24
  hoursFact <- queryPredicate "hours_until_departure" [bookingId]
  let hours = getScalarNum (hoursFact.arguments !! 1)
  hoursCheck <- require (hours > 24) "hours_until_departure > 24"
  let hoursProof = RuleApplied "advance_notice(>24h)" [FactUsed hoursFact, hoursCheck]

  pure $ RuleApplied "fare_allows_refund(full)" [fareProof, hoursProof]

-- | Guard for partial refund: eligible via standard_fare or bereavement.
-- Also requires user confirmation of cancellation understanding.
partialRefundGuard :: Guard
partialRefundGuard args = do
  bookingId <- extractArg "bookingId" args

  -- Check eligibility through one of the partial refund paths
  eligibilityProof <-
    oneOf
      [ do
          proof <- standardFareProof bookingId
          pure $ RuleApplied "eligible_for_refund(partial, standard_fare)" [proof],
        do
          proof <- bereavementProof bookingId
          pure $ RuleApplied "eligible_for_refund(partial, bereavement)" [proof]
      ]

  pure $ RuleApplied "partial_refund_eligibility" [eligibilityProof]

-- | Prove fare_allows_refund(B, partial): fare class is Standard and > 72 hours until departure.
standardFareProof :: Scalar -> SolverM Proof
standardFareProof bookingId = do
  -- Check fare class is standard
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact.arguments !! 1
  fareCheck <- require (fareClass == ScStr "Standard") "fare_class == Standard"
  let fareProof = RuleApplied "standard_fare_class" [FactUsed fareClassFact, fareCheck]

  -- Check hours until departure > 72
  hoursFact <- queryPredicate "hours_until_departure" [bookingId]
  let hours = getScalarNum (hoursFact.arguments !! 1)
  hoursCheck <- require (hours > 72) "hours_until_departure > 72"
  let hoursProof = RuleApplied "advance_notice(>72h)" [FactUsed hoursFact, hoursCheck]

  pure $ RuleApplied "fare_allows_refund(partial)" [fareProof, hoursProof]

-- | Prove bereavement_eligible(B): user claims bereavement and confirms documentation.
-- Note: In a full implementation, this would use askable predicates.
bereavementProof :: Scalar -> SolverM Proof
bereavementProof bookingId = do
  -- For now, this path always fails since we can't yet query askable predicates
  -- In the full implementation, this would check:
  -- - user_claims_bereavement(current_user)
  -- - user_confirms_upload_documentation(current_user, "death_certificate")
  claimFact <- queryPredicate "user_claims_bereavement" [bookingId] -- This will fail/block
  let claimProof = RuleApplied "bereavement_claimed" [FactUsed claimFact]

  -- Would also need documentation proof
  pure $ RuleApplied "bereavement_eligible" [claimProof]

-- | Guard for voucher: eligible for Basic Economy with medical documentation.
voucherGuard :: Guard
voucherGuard args = do
  bookingId <- extractArg "bookingId" args

  -- Check fare class is Basic
  fareClassFact <- queryPredicate "booking_fare_class" [bookingId]
  let fareClass = fareClassFact.arguments !! 1
  fareCheck <- require (fareClass == ScStr "Basic") "fare_class == Basic"
  let fareProof = RuleApplied "basic_fare_class" [FactUsed fareClassFact, fareCheck]

  -- In a full implementation, this would also check:
  -- - user_claims_medical_emergency(current_user)
  -- - user_confirms_upload_documentation(current_user, "medical_certificate")
  -- - user_accepts_voucher_terms(current_user)

  let eligibilityProof = RuleApplied "fare_allows_refund(voucher)" [fareProof]

  pure $ RuleApplied "voucher_eligibility" [eligibilityProof]

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
          arity = 1,
          questionTemplate = "Do you understand that if we process this refund, your booking will be cancelled and your ticket will become invalid?",
          evidenceType = ExplicitConfirmation,
          description = "User confirms they understand the booking will be cancelled"
        },
      AskableDecl
        { predicate = "user_claims_medical_emergency",
          arity = 1,
          questionTemplate = "Are you requesting this refund due to a medical emergency that prevents you from traveling?",
          evidenceType = UserStatement,
          description = "User claims medical emergency"
        },
      AskableDecl
        { predicate = "user_claims_bereavement",
          arity = 1,
          questionTemplate = "Are you requesting this refund due to bereavement (death of an immediate family member)?",
          evidenceType = UserStatement,
          description = "User claims bereavement circumstances"
        },
      AskableDecl
        { predicate = "user_accepts_voucher_terms",
          arity = 1,
          questionTemplate = "Do you accept the terms and conditions of the travel voucher program (voucher valid for 1 year, non-transferable)?",
          evidenceType = ExplicitConfirmation,
          description = "User accepts voucher T&C"
        },
      AskableDecl
        { predicate = "user_confirms_upload_documentation",
          arity = 2,
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
          candidateQuery = Nothing,
          seedValue = Just (FromSession "user_id"),
          description = "The authenticated user's ID"
        },
      ContextDecl
        { name = "booking_of_interest",
          candidateQuery = Just "user_bookings",
          seedValue = Nothing,
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
