-- | Air Canada Sentinel: guards tool calls and manages fact resolution.
--
-- This is a monolithic implementation that knows about all Air Canada tools,
-- their guards, and how to resolve missing facts by invoking data tools.
--
-- The Sentinel sits between the Agent and tool execution:
-- 1. Agent requests a tool call via 'guardedCall'
-- 2. Sentinel evaluates the guard
-- 3. If blocked on missing facts, Sentinel tries to establish them via data tools
-- 4. If still blocked and askable, returns 'AskUser'
-- 5. If allowed, executes the tool and returns 'Allowed'
-- 6. If denied, returns 'Denied'
module Examples.AirCanada.Sentinel
  ( -- * Sentinel
    airCanadaSentinel,

    -- * Type aliases
    AirCanadaSentinelM,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text qualified as Aeson.Text
import Data.Text qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Examples.AirCanada.Facts qualified as Facts
import Examples.AirCanada.MockDB (attemptRefund, getBooking, getFlight, listBookingsForPassenger)
import Examples.AirCanada.Refund (BookingSource (..), DeathCircumstance (..), SpecialException (..))
import Examples.AirCanada.Types
import Pre
import Sentinel.Facts qualified as FactsDB
import Sentinel.Guard qualified as Guard
import Sentinel.Output qualified as Output
import Sentinel.Sentinel (Sentinel (..), SentinelM, SentinelResult (..), addFacts, getDb, getFacts, modifyDb)

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

-- | Air Canada Sentinel monad.
type AirCanadaSentinelM = SentinelM AirlineDB Facts.Fact

--------------------------------------------------------------------------------
-- Sentinel Implementation
--------------------------------------------------------------------------------

-- | The Air Canada Sentinel.
--
-- This bundles the guardedCall and summarizeFacts implementations.
airCanadaSentinel :: Sentinel AirlineDB Facts.Fact
airCanadaSentinel =
  Sentinel
    { guardedCall = guardedCallImpl,
      summarizeFacts = summarizeFactsImpl
    }

-- | Maximum attempts to resolve a blocked guard by running queries.
maxResolutionAttempts :: Int
maxResolutionAttempts = 3

-- | Implementation of guardedCall for Air Canada.
guardedCallImpl :: Text -> Aeson.Value -> AirCanadaSentinelM SentinelResult
guardedCallImpl toolName args = do
  liftIO $ putDispLn (Output.ToolUse toolName (T.Lazy.toStrict $ Aeson.Text.encodeToLazyText args))
  resolveAndExecute maxResolutionAttempts [] toolName args

-- | Resolve guard and execute tool, with automatic fact establishment.
-- Tracks query failures to provide detailed error messages.
resolveAndExecute :: Int -> [Text] -> Text -> Aeson.Value -> AirCanadaSentinelM SentinelResult
resolveAndExecute attemptsRemaining queryFailures toolName args = do
  currentFacts <- getFacts
  -- Get the guard for this tool
  let guardAction = getGuard toolName args
  -- Evaluate the guard
  result <- liftIO $ Guard.evaluateGuard currentFacts guardAction
  case result of
    Guard.GuardAllowed -> do
      liftIO $ putDispLn (Output.GuardPass toolName)
      -- Execute the tool
      executeToolAction toolName args
    Guard.GuardDenied failure -> do
      -- Include query failures in the error message if any
      let baseReason = formatGuardFailure failure
          reason = case queryFailures of
            [] -> baseReason
            _ -> baseReason <> ". Resolution attempted: " <> T.intercalate "; " queryFailures
      liftIO $ putDispLn (Output.GuardDenied toolName reason)
      pure $ Denied reason
    Guard.NeedsResolution (Guard.RunQueries queries) | attemptsRemaining > 0 -> do
      -- Display resolution attempt
      let attemptNum = maxResolutionAttempts - attemptsRemaining + 1
      liftIO $ putDispLn (Output.ResolutionAttempt toolName attemptNum (length queries))
      -- Execute each pending query, collecting failures
      results <- traverse executeQueryWithFailure (toList queries)
      let (successes, failures) = partitionResults results
          newFailures = queryFailures <> failures
      -- If at least one query succeeded, retry the guard
      if not (null successes)
        then resolveAndExecute (attemptsRemaining - 1) newFailures toolName args
        else do
          let reason = "Could not establish required facts: " <> T.intercalate "; " newFailures
          liftIO $ putDispLn (Output.GuardDenied toolName reason)
          pure $ Denied reason
    Guard.NeedsResolution (Guard.RunQueries queries) -> do
      -- Resolution attempts exhausted - include what we tried
      let queryDescs = T.intercalate ", " ((.queryDescription) <$> toList queries)
          reason = "Resolution attempts exhausted. Still need: " <> queryDescs
                   <> (if null queryFailures then "" else ". Previous failures: " <> T.intercalate "; " queryFailures)
      liftIO $ putDispLn (Output.GuardDenied toolName reason)
      pure $ Denied reason
    Guard.NeedsResolution (Guard.AskUser question) -> do
      -- User input is needed as a last resort
      liftIO $ putDispLn (Output.NeedsUserInput toolName question.questionText)
      pure $ AskUser question

-- | Result of executing a query: either success or failure description.
data QueryResult = QuerySuccess | QueryFailure Text

-- | Execute a query and return success or failure description.
executeQueryWithFailure :: Guard.PendingQuery -> AirCanadaSentinelM QueryResult
executeQueryWithFailure query = do
  let queryName = query.queryToolName
      queryArgs = query.queryArgs
      argsText = T.Lazy.toStrict $ Aeson.Text.encodeToLazyText queryArgs

  -- Display the query being executed
  liftIO $ putDispLn (Output.QueryExecution queryName argsText)

  -- Execute the query action
  result <- runExceptT (executeDataToolAction queryName queryArgs)
  case result of
    Left err -> do
      liftIO $ putDispLn (Output.ToolError err)
      pure $ QueryFailure (queryName <> ": " <> err)
    Right _obs -> do
      -- Extract and add facts from the query result
      producedFacts <- produceFactsFor queryName queryArgs
      addFacts producedFacts
      pure QuerySuccess

-- | Partition query results into successes and failure descriptions.
partitionResults :: [QueryResult] -> ([()], [Text])
partitionResults = foldr go ([], [])
  where
    go QuerySuccess (ss, fs) = (() : ss, fs)
    go (QueryFailure f) (ss, fs) = (ss, f : fs)

-- | Format a guard failure for display.
formatGuardFailure :: Guard.GuardFailure -> Text
formatGuardFailure failure =
  case failure.reasons of
    [] -> "Guard failed with no specific reason"
    reasons -> T.intercalate "; " (formatReason <$> reasons)
  where
    formatReason :: Guard.FailureReason -> Text
    formatReason = \case
      Guard.MissingFact desc -> "Missing fact: " <> desc
      Guard.ForbiddenFact desc -> "Forbidden: " <> desc
      Guard.ExplicitDenial reason -> reason
      Guard.QueryFailed tool err -> "Query " <> tool <> " failed: " <> err

--------------------------------------------------------------------------------
-- Guards
--------------------------------------------------------------------------------

-- | Get the guard for a tool.
getGuard :: Text -> Aeson.Value -> Guard.GuardM Facts.Fact ()
getGuard toolName = case toolName of
  "Login" -> const (pure ()) -- No guard for login - always allowed
  "RetrieveBooking" -> userIdentityGuard
  "CheckFlightStatus" -> userIdentityGuard
  "SearchBookingsByName" -> searchBookingsGuard
  "InitiateRefund" -> refundGuard
  _ -> \_ -> Guard.denyWith $ "Unknown tool: " <> toolName

-- | Guard that requires user identity to be established.
-- All query tools should use this to ensure we know who the user is.
userIdentityGuard :: Aeson.Value -> Guard.GuardM Facts.Fact ()
userIdentityGuard _ = do
  -- Check if we have established who the user is
  facts <- Guard.queryFacts \case
    Facts.LoggedInUser _ -> True
    _ -> False
  case facts of
    (_ : _) -> pure ()
    [] -> Guard.denyWith "User is not logged in. Use the Login tool to establish user identity first."

-- | Guard for SearchBookingsByName - requires identity AND name must match logged-in user.
searchBookingsGuard :: Aeson.Value -> Guard.GuardM Facts.Fact ()
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
      Guard.forbidFactMatching "Booking is from a travel agency (cannot process refunds)" \case
        Facts.BookingSource bRef TravelAgency -> bRef == normalizedRef
        _ -> False
      -- Forbid bookings from other airlines (we can't process those)
      Guard.forbidFactMatching "Booking is from another airline (cannot process refunds)" \case
        Facts.BookingSource bRef OtherAirline -> bRef == normalizedRef
        _ -> False

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Execute a tool action and return the result.
executeToolAction :: Text -> Aeson.Value -> AirCanadaSentinelM SentinelResult
executeToolAction toolName args = do
  result <- runExceptT (executeToolActionImpl toolName args)
  case result of
    Left err -> do
      liftIO $ putDispLn (Output.ToolError err)
      pure $ Denied err
    Right obs -> do
      -- Collect facts produced by the tool
      producedFacts <- produceFactsFor toolName args
      addFacts producedFacts
      liftIO $ putDispLn (Output.Observation obs)
      pure $ Allowed obs

-- | Execute the actual tool action (internal implementation).
executeToolActionImpl :: Text -> Aeson.Value -> ExceptT Text AirCanadaSentinelM Text
executeToolActionImpl toolName args = case toolName of
  "Login" -> do
    userName <- extractString "userName" args ??: "Missing or invalid 'userName' parameter"
    pure $ "Successfully logged in as: " <> userName
  "RetrieveBooking" -> do
    ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
    db <- lift getDb
    case getBooking ref db of
      Just booking -> pure $ renderDocPlain (disp booking)
      Nothing -> throwError $ "No booking found with reference: " <> ref
  "CheckFlightStatus" -> do
    flightNum <- extractString "flightNumber" args ??: "Missing or invalid 'flightNumber' parameter"
    db <- lift getDb
    case getFlight flightNum db of
      Just flight -> pure $ renderDocPlain (disp flight)
      Nothing -> throwError $ "No flight found with number: " <> flightNum
  "SearchBookingsByName" -> do
    name <- extractString "passengerName" args ??: "Missing or invalid 'passengerName' parameter"
    db <- lift getDb
    case listBookingsForPassenger name db of
      [] -> throwError $ "No bookings found for passenger: " <> name
      bookings ->
        pure
          $ renderDocPlain
          $ vsep
            [ "Found" <+> pretty (length bookings) <+> "booking(s):",
              mempty,
              vsep (punctuate (line <> "---" <> line) (fmap disp bookings))
            ]
  "InitiateRefund" -> do
    ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
    let specialReason = case extractString "reason" args of
          Just "jury" -> Just JuryDuty
          Just "military" -> Just MilitaryOrders
          Just "death" -> Just (Death PassengerDeath)
          _ -> Nothing
    db <- lift getDb
    let (result, updatedDB) = attemptRefund (T.toUpper ref) specialReason db
    lift $ modifyDb (const updatedDB)
    pure result
  _ -> throwError $ "Unknown tool: " <> toolName

-- | Execute a data tool action (for query resolution).
-- This is similar to executeToolActionImpl but only for data tools.
executeDataToolAction :: Text -> Aeson.Value -> ExceptT Text AirCanadaSentinelM Text
executeDataToolAction toolName args = case toolName of
  "RetrieveBooking" -> do
    ref <- extractString "bookingRef" args ??: "Missing or invalid 'bookingRef' parameter"
    db <- lift getDb
    case getBooking ref db of
      Just booking -> pure $ renderDocPlain (disp booking)
      Nothing -> throwError $ "No booking found with reference: " <> ref
  "CheckFlightStatus" -> do
    flightNum <- extractString "flightNumber" args ??: "Missing or invalid 'flightNumber' parameter"
    db <- lift getDb
    case getFlight flightNum db of
      Just flight -> pure $ renderDocPlain (disp flight)
      Nothing -> throwError $ "No flight found with number: " <> flightNum
  "SearchBookingsByName" -> do
    name <- extractString "passengerName" args ??: "Missing or invalid 'passengerName' parameter"
    db <- lift getDb
    case listBookingsForPassenger name db of
      [] -> throwError $ "No bookings found for passenger: " <> name
      bookings ->
        pure
          $ renderDocPlain
          $ vsep
            [ "Found" <+> pretty (length bookings) <+> "booking(s):",
              mempty,
              vsep (punctuate (line <> "---" <> line) (fmap disp bookings))
            ]
  _ -> throwError $ "Not a data tool: " <> toolName

--------------------------------------------------------------------------------
-- Fact Production
--------------------------------------------------------------------------------

-- | Produce facts for a tool invocation.
produceFactsFor :: Text -> Aeson.Value -> AirCanadaSentinelM [Facts.Fact]
produceFactsFor toolName args = do
  db <- getDb
  pure $ case toolName of
    "Login" ->
      case extractString "userName" args of
        Nothing -> []
        Just userName -> [Facts.LoggedInUser userName]
    "RetrieveBooking" ->
      case extractString "bookingRef" args of
        Nothing -> []
        Just ref -> case getBooking ref db of
          Nothing -> []
          Just booking -> bookingToFacts booking
    "CheckFlightStatus" ->
      case extractString "flightNumber" args of
        Nothing -> []
        Just flightNum -> case getFlight flightNum db of
          Nothing -> []
          Just flight -> flightToFacts flight
    "SearchBookingsByName" ->
      case extractString "passengerName" args of
        Nothing -> []
        Just name -> concatMap bookingToFacts (listBookingsForPassenger name db)
    "InitiateRefund" -> [] -- Action tools produce no facts
    _ -> []

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
-- Summarize Facts
--------------------------------------------------------------------------------

-- | Summarize current facts for the LLM context.
summarizeFactsImpl :: AirCanadaSentinelM Text
summarizeFactsImpl = do
  factsDb <- getFacts
  let allFacts = FactsDB.allFacts factsDb
  if null allFacts
    then pure "No facts established yet."
    else pure $ T.unlines $ "Known facts:" : fmap (("  - " <>) . T.pack . show) allFacts

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
