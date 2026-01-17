module Examples.AirCanada
  ( TicketType (..),
    BookingSource (..),
    InvoluntaryReason (..),
    SpecialException (..),
    DeathCircumstance (..),
    TicketFormat (..),
    TicketUsage (..),
    Money (..),
    RefundOutcome (..),
    Ticket (..),
    RefundReason (..),
    RefundRequest (..),
    ExpiredCreditFee (..),
    calculateRefund,
    applyPaperTicketFee,
    isCreditExpired,
    isWithin24Hours,
    subtractMoney,
    twentyFourHours,
    lostPaperTicketFee,
    expiredCreditFee,
  )
where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Pre

-- | Types of tickets
data TicketType
  = EconomyBasic
  | OtherNonRefundable
  | Refundable
  deriving stock (Eq, Show)

-- | Where the ticket was purchased
data BookingSource
  = DirectAirCanada
  | TravelAgency
  | OtherAirline
  | GroupBooking
  deriving stock (Eq, Show)

-- | Reasons for involuntary refund (Air Canada's fault)
data InvoluntaryReason
  = FlightCancelled
  | FlightDelayed
  | DeniedBoarding
  | DowngradedClass
  deriving stock (Eq, Show)

-- | Special exception reasons
data SpecialException
  = JuryDuty
  | MilitaryOrders
  | Death DeathCircumstance
  deriving stock (Eq, Show)

-- | Death-related circumstances
data DeathCircumstance
  = PassengerDeath
  | ImmediateFamilyDeath
  | TravelCompanionDeath
  | VisitingDeceased
  deriving stock (Eq, Show)

-- | Ticket format
data TicketFormat
  = Electronic
  | Paper
  deriving stock (Eq, Show)

-- | Usage status of a ticket
data TicketUsage
  = Unused
  | -- | Value of flown segments
    PartiallyUsed Money
  | FullyUsed
  deriving stock (Eq, Show)

-- | Money amount in cents to avoid floating point issues
newtype Money = Money {cents :: Int}
  deriving stock (Eq, Show, Ord)

instance Semigroup Money where
  Money a <> Money b = Money (a + b)

instance Monoid Money where
  mempty = Money 0

-- | Refund outcome
data RefundOutcome
  = FullRefund Money
  | PartialRefund Money
  | TravelCredit Money
  | ACWalletFunds Money
  | NoRefund
  | ContactAgency BookingSource
  | RefundPlusFreeReturn Money
  deriving stock (Eq, Show)

-- | A ticket with all relevant information
data Ticket = Ticket
  { ticketType :: TicketType,
    ticketFormat :: TicketFormat,
    pricePaid :: Money,
    purchaseTime :: UTCTime,
    bookingSource :: BookingSource,
    usage :: TicketUsage,
    cancellationPenalty :: Money
  }
  deriving stock (Eq, Show)

-- | Reason for requesting a refund
data RefundReason
  = Involuntary InvoluntaryReason
  | Voluntary
  | SpecialCase SpecialException
  deriving stock (Eq, Show)

-- | Context for a refund request
data RefundRequest = RefundRequest
  { ticket :: Ticket,
    reason :: RefundReason,
    requestTime :: UTCTime,
    tripNowPointless :: Bool,
    hasDocumentation :: Bool -- for special exceptions
  }
  deriving stock (Eq, Show)

-- Constants

twentyFourHours :: NominalDiffTime
twentyFourHours = 24 * 60 * 60

-- | Lost paper ticket fee ($100)
lostPaperTicketFee :: Money
lostPaperTicketFee = Money 10000

-- | Check if a refund request is within 24 hours of purchase
isWithin24Hours :: RefundRequest -> Bool
isWithin24Hours req =
  diffUTCTime req.requestTime req.ticket.purchaseTime <= twentyFourHours

-- | Subtract money, flooring at zero
subtractMoney :: Money -> Money -> Money
subtractMoney (Money a) (Money b) = Money (max 0 (a - b))

-- | Calculate refund for an involuntary reason
calculateInvoluntaryRefund :: RefundRequest -> InvoluntaryReason -> RefundOutcome
calculateInvoluntaryRefund req involuntaryReason =
  let t = req.ticket
      price = t.pricePaid
   in case (t.usage, involuntaryReason, req.tripNowPointless) of
        -- Downgrade: refund the difference
        (_, DowngradedClass, _) ->
          PartialRefund t.cancellationPenalty -- penalty field repurposed as difference

        -- Trip is now pointless: full refund + free return
        (_, _, True) ->
          RefundPlusFreeReturn price
        -- Haven't flown yet: full refund of unused portion
        (Unused, _, _) ->
          FullRefund price
        -- Partially used: refund unused portion
        (PartiallyUsed flownValue, _, _) ->
          PartialRefund (subtractMoney price flownValue)
        -- Fully used: nothing to refund
        (FullyUsed, _, _) ->
          NoRefund

-- | Calculate refund for a voluntary cancellation
calculateVoluntaryRefund :: RefundRequest -> RefundOutcome
calculateVoluntaryRefund req =
  let t = req.ticket
      price = t.pricePaid
   in case t.ticketType of
        -- Economy Basic: no refund ever
        EconomyBasic -> NoRefund
        -- Within 24 hours: full refund regardless of ticket type
        _ | isWithin24Hours req -> FullRefund price
        -- Other non-refundable after 24 hours: travel credit only
        OtherNonRefundable ->
          case t.usage of
            Unused -> TravelCredit price
            PartiallyUsed flownValue -> TravelCredit (subtractMoney price flownValue)
            FullyUsed -> NoRefund
        -- Refundable ticket after 24 hours
        Refundable ->
          case t.usage of
            Unused ->
              PartialRefund (subtractMoney price t.cancellationPenalty)
            PartiallyUsed flownValue ->
              let remaining = subtractMoney price flownValue
               in PartialRefund (subtractMoney remaining t.cancellationPenalty)
            FullyUsed ->
              NoRefund

-- | Calculate refund for special exceptions
calculateSpecialExceptionRefund :: RefundRequest -> SpecialException -> RefundOutcome
calculateSpecialExceptionRefund req exception =
  let t = req.ticket
      price = t.pricePaid
   in if not req.hasDocumentation
        then NoRefund -- documentation required
        else case exception of
          JuryDuty -> FullRefund price
          MilitaryOrders -> FullRefund price
          Death _ -> FullRefund price

-- | Main refund calculation function
calculateRefund :: RefundRequest -> RefundOutcome
calculateRefund req =
  let t = req.ticket
   in case t.bookingSource of
        -- Agency/other airline bookings must be processed through them
        TravelAgency -> ContactAgency TravelAgency
        OtherAirline -> ContactAgency OtherAirline
        -- Direct or group bookings
        _ -> case req.reason of
          Involuntary r -> calculateInvoluntaryRefund req r
          Voluntary -> calculateVoluntaryRefund req
          SpecialCase exc -> calculateSpecialExceptionRefund req exc

-- | Apply lost paper ticket fee if applicable
applyPaperTicketFee :: Ticket -> Bool -> RefundOutcome -> RefundOutcome
applyPaperTicketFee t isLost outcome
  | t.ticketFormat == Paper && isLost =
      case outcome of
        FullRefund m -> FullRefund (subtractMoney m lostPaperTicketFee)
        PartialRefund m -> PartialRefund (subtractMoney m lostPaperTicketFee)
        TravelCredit m -> TravelCredit (subtractMoney m lostPaperTicketFee)
        ACWalletFunds m -> ACWalletFunds (subtractMoney m lostPaperTicketFee)
        other -> other
  | otherwise = outcome

-- | Check if a travel credit is expired (1 year)
isCreditExpired :: UTCTime -> UTCTime -> Bool
isCreditExpired issueTime currentTime =
  diffUTCTime currentTime issueTime > oneYear
  where
    oneYear = 365 * 24 * 60 * 60

-- | Fee range for using expired credit ($50-$100)
data ExpiredCreditFee = ExpiredCreditFee
  { minFee :: Money,
    maxFee :: Money
  }
  deriving stock (Eq, Show)

expiredCreditFee :: ExpiredCreditFee
expiredCreditFee = ExpiredCreditFee (Money 5000) (Money 10000)
