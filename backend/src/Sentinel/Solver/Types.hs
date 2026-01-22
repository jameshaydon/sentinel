-- | Core types for the Sentinel solver.
--
-- The solver uses a simple type system based on 'Scalar' values rather than
-- arbitrary JSON. This keeps the solver's core types well-defined.
module Sentinel.Solver.Types
  ( -- * Scalar Values
    Scalar (..),
    ScalarType (..),
    scalarToJSON,
    scalarFromJSON,
    dispScalar,
    scalarToText,

    -- * Base Facts
    BaseFact (..),

    -- * Proof Traces
    Proof (..),
    ProofTree (..),
    ProofResult (..),

    -- * Solver Results
    SolverResult (..),
    SolverSuccess (..),
    UserInputBlock (..),
    UserInputType (..),
    FailurePath (..),
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Pre

--------------------------------------------------------------------------------
-- Scalar Values
--------------------------------------------------------------------------------

-- | A scalar value in the solver's type system.
--
-- The solver uses a simple scalar type rather than arbitrary JSON 'Value'.
-- This keeps pattern matching, equality, and ordering well-defined.
--
-- Constructor names use "Sc" prefix to avoid conflicts with Prettyprinter's SText.
data Scalar
  = ScBool Bool
  | ScNum Double
  | ScStr Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type of a scalar value (for schema validation).
data ScalarType
  = TextType
  | IntType
  | BoolType
  | FloatType
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert a scalar to JSON for tool invocation.
scalarToJSON :: Scalar -> Value
scalarToJSON = \case
  ScBool b -> Aeson.Bool b
  ScNum n -> Aeson.Number (realToFrac n)
  ScStr t -> Aeson.String t

-- | Try to parse a JSON value as a scalar.
scalarFromJSON :: Value -> Maybe Scalar
scalarFromJSON = \case
  Aeson.Bool b -> Just (ScBool b)
  Aeson.Number n -> Just (ScNum (realToFrac n))
  Aeson.String t -> Just (ScStr t)
  _ -> Nothing

-- | Display a scalar for pretty-printing (proof traces, diagnostics).
-- Uses "true"/"false" for bools and quotes strings.
dispScalar :: Scalar -> Doc Ann
dispScalar = \case
  ScBool b -> if b then "true" else "false"
  ScNum n -> pretty (show n)
  ScStr t -> dquotes (pretty t)

-- | Convert a scalar to text (for question formatting, descriptions).
-- Uses "yes"/"no" for bools and plain strings (no quotes).
scalarToText :: Scalar -> Text
scalarToText = \case
  ScBool True -> "yes"
  ScBool False -> "no"
  ScNum n -> fromString (show n)
  ScStr t -> t

--------------------------------------------------------------------------------
-- Base Facts
--------------------------------------------------------------------------------

-- | A base fact is a ground predicate established by tool output.
--
-- Base facts have a predicate name and a list of scalar arguments.
-- For example: @BaseFact "flight_status" [ScStr "AL-445", ScStr "delayed"]@
data BaseFact = BaseFact
  { predicateName :: Text,
    arguments :: [Scalar]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp BaseFact where
  disp fact =
    pretty fact.predicateName
      <> parens (hsep (punctuate comma (dispScalar <$> fact.arguments)))

--------------------------------------------------------------------------------
-- Proof Traces
--------------------------------------------------------------------------------

-- | A proof trace showing how a goal was established.
--
-- Proofs form a tree structure where each node records either:
-- - Use of an existing fact
-- - Application of a named rule with sub-proofs for premises
-- - Resolution of a context variable
data Proof
  = -- | Used an existing fact from the store
    FactUsed BaseFact
  | -- | Applied a named rule with sub-proofs for premises
    RuleApplied Text [Proof]
  | -- | Resolved a context variable to a value
    ContextBound Text Scalar
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Proof where
  disp = \case
    FactUsed fact -> "fact:" <+> disp fact
    RuleApplied name proofs ->
      vsep
        [ "rule:" <+> pretty name,
          indent 2 (vsep (disp <$> proofs))
        ]
    ContextBound name value ->
      "context:" <+> pretty name <+> "=" <+> dispScalar value

-- | A proof tree with the goal and its result.
data ProofTree = ProofTree
  { goal :: Text,
    result :: ProofResult
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The result of attempting to prove a goal.
data ProofResult
  = -- | Successfully proven with a complete proof
    Proven Proof
  | -- | Failed to prove, with reason and partial progress
    Failed Text (Maybe Proof)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Solver Results
--------------------------------------------------------------------------------

-- | The result of running the solver on a query.
data SolverResult
  = -- | One or more successful proof paths
    Success (NonEmpty SolverSuccess)
  | -- | Blocked waiting for user input (context or askable)
    BlockedOnUserInput UserInputBlock
  | -- | All paths failed
    Failure [FailurePath]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A successful proof with bindings and explanation.
data SolverSuccess = SolverSuccess
  { -- | Variable bindings discovered during proof (e.g., RefundType -> "full")
    bindings :: Map Text Scalar,
    -- | The proof trace
    proof :: Proof,
    -- | Human-readable reason (e.g., "airline_fault")
    reason :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp SolverSuccess where
  disp success =
    vsep
      [ label "Reason:" <+> pretty success.reason,
        label "Bindings:" <+> dispBindings success.bindings,
        label "Proof:",
        indent 2 (disp success.proof)
      ]
    where
      dispBindings m =
        encloseSep lbrace rbrace ", "
          $ [ pretty k <+> "=" <+> dispScalar v
            | (k, v) <- M.toList m
            ]

-- | The type of user input being requested.
data UserInputType
  = -- | Context variable (e.g., "booking_of_interest")
    ContextInput
  | -- | Askable predicate (e.g., "user_claims_bereavement")
    AskableInput
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Unified information about a blocked user input request.
--
-- This replaces the separate ContextBlock and AskableBlock types
-- with a unified structure that captures both cases.
data UserInputBlock = UserInputBlock
  { -- | The type of input being requested
    inputType :: UserInputType,
    -- | The name of the context variable or askable predicate
    name :: Text,
    -- | Human-readable question to ask the user
    question :: Text,
    -- | Arguments (empty for context, predicate args for askable)
    arguments :: [Scalar],
    -- | Candidate values (for context with known candidates)
    candidates :: [Scalar],
    -- | Partial proof progress so far
    partialProof :: Maybe Proof
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp UserInputBlock where
  disp block =
    vsep
      [ label "Blocked on:" <+> dispInputType block.inputType <+> pretty block.name,
        indent 2 (label "Question:" <+> pretty block.question),
        case block.arguments of
          [] -> mempty
          args ->
            indent 2
              $ label "Arguments:"
              <+> hsep (punctuate comma (dispScalar <$> args)),
        case block.candidates of
          [] -> mempty
          cs ->
            indent 2
              $ label "Candidates:"
              <+> hsep (punctuate comma (dispScalar <$> cs)),
        case block.partialProof of
          Nothing -> mempty
          Just p -> vsep [indent 2 (label "Progress so far:"), indent 4 (disp p)]
      ]
    where
      dispInputType ContextInput = "context"
      dispInputType AskableInput = "askable"

-- | A failed proof path with explanation.
data FailurePath = FailurePath
  { -- | The rule that was being tried
    ruleName :: Text,
    -- | Why it failed
    reason :: Text,
    -- | Partial proof progress before failure
    partialProof :: Maybe Proof
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp FailurePath where
  disp fp =
    vsep
      [ errorText "Failed:" <+> pretty fp.ruleName,
        indent 2 (pretty fp.reason),
        case fp.partialProof of
          Nothing -> mempty
          Just p -> indent 2 ("Progress:" <+> disp p)
      ]

instance Disp ProofTree where
  disp pt =
    vsep
      [ label "Goal:" <+> pretty pt.goal,
        disp pt.result
      ]

instance Disp ProofResult where
  disp = \case
    Proven proof -> vsep [successText "Proven:", indent 2 (disp proof)]
    Failed reason mProof ->
      vsep
        [ errorText "Failed:" <+> pretty reason,
          case mProof of
            Nothing -> mempty
            Just p -> indent 2 ("Progress:" <+> disp p)
        ]

instance Disp SolverResult where
  disp = \case
    Success successes ->
      vsep
        [ successText "Success:" <+> pretty (length successes) <+> "proof path(s)",
          vsep (toList (numberSuccesses successes))
        ]
      where
        numberSuccesses :: NonEmpty SolverSuccess -> NonEmpty (Doc Ann)
        numberSuccesses ss =
          zipWith
            (\i s -> vsep [subheader ("Path" <+> pretty i <> ":"), indent 2 (disp s)])
            ([1 ..] :: [Int])
            (toList ss)
            & NE.fromList
    BlockedOnUserInput block -> disp block
    Failure failures ->
      vsep
        [ errorText "All paths failed:" <+> pretty (length failures) <+> "path(s) tried",
          vsep (disp <$> failures)
        ]
