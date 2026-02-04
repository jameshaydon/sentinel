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
    factOutput,

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

    -- * Solver Outcome
    SolverOutcome (..),
    formatSolverOutcomeForLLM,
    dedupBlocks,
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as Aeson.Types
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Text qualified as T
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
  | ScExpr Text [Scalar]
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Scalar where
  toJSON = \case
    ScBool b -> Aeson.object ["tag" Aeson..= ("ScBool" :: Text), "contents" Aeson..= b]
    ScNum n -> Aeson.object ["tag" Aeson..= ("ScNum" :: Text), "contents" Aeson..= n]
    ScStr t -> Aeson.object ["tag" Aeson..= ("ScStr" :: Text), "contents" Aeson..= t]
    ScExpr name args -> Aeson.object ["tag" Aeson..= ("ScExpr" :: Text), "contents" Aeson..= Aeson.toJSON (name, args)]

instance FromJSON Scalar where
  parseJSON = Aeson.withObject "Scalar" \o -> do
    tag <- o Aeson..: "tag" :: Aeson.Types.Parser Text
    case tag of
      "ScBool" -> ScBool <$> o Aeson..: "contents"
      "ScNum" -> ScNum <$> o Aeson..: "contents"
      "ScStr" -> ScStr <$> o Aeson..: "contents"
      "ScExpr" -> do
        (name, args) <- o Aeson..: "contents"
        pure $ ScExpr name args
      _ -> fail $ "Unknown Scalar tag: " <> T.unpack tag

-- | Type of a scalar value (for schema validation).
data ScalarType
  = TextType
  | IntType
  | BoolType
  | FloatType
  | ExprType
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert a scalar to JSON for tool invocation.
scalarToJSON :: Scalar -> Value
scalarToJSON = \case
  ScBool b -> Aeson.Bool b
  ScNum n -> Aeson.Number (realToFrac n)
  ScStr t -> Aeson.String t
  ScExpr name [] -> Aeson.Object (KM.singleton (Key.fromText "_fn") (Aeson.String name))
  ScExpr name args -> Aeson.Object (KM.fromList [(Key.fromText "_fn", Aeson.String name), (Key.fromText "_args", Aeson.toJSON (map scalarToJSON args))])

-- | Try to parse a JSON value as a scalar.
scalarFromJSON :: Value -> Maybe Scalar
scalarFromJSON = \case
  Aeson.Bool b -> Just (ScBool b)
  Aeson.Number n -> Just (ScNum (realToFrac n))
  Aeson.String t -> Just (ScStr t)
  Aeson.Object obj
    | Just (Aeson.String name) <- KM.lookup (Key.fromText "_fn") obj ->
        let args = case KM.lookup (Key.fromText "_args") obj of
              Just (Aeson.Array arr) -> traverse scalarFromJSON (toList arr)
              Nothing -> Just []
              _ -> Nothing
         in ScExpr name <$> args
  _ -> Nothing

-- | Display instance for scalars.
-- Uses "true"/"false" for bools and plain strings (no quotes).
instance Disp Scalar where
  disp = \case
    ScBool b -> if b then "true" else "false"
    ScNum n -> pretty (show n)
    ScStr t -> pretty t
    ScExpr name [] -> pretty name
    ScExpr name args -> pretty name <> parens (hsep (punctuate comma (disp <$> args)))

-- | Display a scalar for pretty-printing (proof traces, diagnostics).
-- Uses "true"/"false" for bools and quotes strings.
dispScalar :: Scalar -> Doc Ann
dispScalar = \case
  ScBool b -> if b then "true" else "false"
  ScNum n -> pretty (show n)
  ScStr t -> dquotes (pretty t)
  ScExpr name [] -> pretty name
  ScExpr name args -> pretty name <> parens (hsep (punctuate comma (dispScalar <$> args)))

-- | Convert a scalar to text (for question formatting, descriptions).
-- Uses "yes"/"no" for bools and plain strings (no quotes).
scalarToText :: Scalar -> Text
scalarToText = \case
  ScBool True -> "yes"
  ScBool False -> "no"
  ScNum n -> fromString (show n)
  ScStr t -> t
  ScExpr name [] -> name
  ScExpr name args -> name <> "(" <> T.intercalate ", " (map scalarToText args) <> ")"

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

-- | Extract the single output argument from a binary fact (1 input, 1 output).
factOutput :: BaseFact -> Scalar
factOutput fact = case fact.arguments of
  [_, output] -> output
  _ ->
    error $
      "factOutput: expected 2 arguments in "
        <> T.unpack fact.predicateName
        <> " fact, got "
        <> show (length fact.arguments)

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
  | -- | Applied a named rule with arguments and sub-proofs for premises
    RuleApplied Text [Scalar] [Proof]
  | -- | Resolved a context variable to a value
    ContextBound Text Scalar
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp Proof where
  disp = \case
    FactUsed fact -> "fact:" <+> disp fact
    RuleApplied name args proofs ->
      vsep
        [ "rule:" <+> case args of
            [] -> pretty name
            _ -> pretty name <> parens (hsep (punctuate comma (dispScalar <$> args))),
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

--------------------------------------------------------------------------------
-- Solver Outcome
--------------------------------------------------------------------------------

-- | The full outcome of a solver run: all proofs, all blocks, and all failures.
--
-- Unlike 'SolverResult' which collapses to a single case, 'SolverOutcome'
-- captures everything the solver discovered in a single run. This is useful for
-- providing complete information to both the LLM and the console display.
data SolverOutcome = SolverOutcome
  { -- | Name of the goal that was being proved
    goalName :: Text,
    -- | All successful proofs found
    successes :: [SolverSuccess],
    -- | All blocked user input requests (deduplicated)
    blocked :: [UserInputBlock],
    -- | All failed proof paths
    failures :: [FailurePath]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Disp SolverOutcome where
  disp outcome =
    vsep $
      catMaybes
        [ Just $ label "Goal:" <+> pretty outcome.goalName,
          proofSection,
          blockSection,
          failureSection
        ]
    where
      proofSection = case outcome.successes of
        [] -> Nothing
        proofs ->
          Just $
            vsep
              [ successText (pretty (length proofs) <+> "proof(s) found:"),
                indent 2 (vsep (zipWith dispProof [1 :: Int ..] proofs))
              ]
      dispProof i s =
        vsep [label ("Proof" <+> pretty i <> ":"), indent 2 (disp s)]

      blockSection = case outcome.blocked of
        [] -> Nothing
        blocks ->
          Just $
            vsep
              [ label (pretty (length blocks) <+> "question(s) blocking further exploration:"),
                indent 2 (vsep [label "-" <+> pretty b.question | b <- blocks])
              ]

      failureSection = case (outcome.successes, outcome.blocked, outcome.failures) of
        ([], [], fs@(_ : _)) ->
          Just $
            vsep
              [ errorText "All paths failed:" <+> pretty (length fs) <+> "path(s) tried",
                indent 2 (vsep (disp <$> fs))
              ]
        _ -> Nothing

-- | Format a solver outcome as text for the LLM observation.
--
-- Uses 'dispText' to render proof trees as plain text. Handles the four
-- cases: (no proofs, no blocks), (proofs only), (blocks only), (both).
formatSolverOutcomeForLLM :: SolverOutcome -> Text
formatSolverOutcomeForLLM outcome =
  case (outcome.successes, outcome.blocked) of
    ([], []) ->
      "Solver: " <> outcome.goalName <> " -- DENIED\n"
        <> "  All paths failed.\n"
        <> formatFailuresForLLM outcome.failures
    ([], blocks) ->
      "Solver: " <> outcome.goalName <> " -- BLOCKED\n"
        <> "  0 proof(s) found.\n"
        <> "  " <> tshow (length blocks) <> " question(s) blocking:\n"
        <> T.unlines ["    - " <> b.question | b <- blocks]
    (proofs, []) ->
      "Solver: " <> outcome.goalName <> "\n"
        <> "  " <> tshow (length proofs) <> " proof(s) found:\n"
        <> formatProofsForLLM proofs
        <> "\n  All paths fully explored."
    (proofs, blocks) ->
      "Solver: " <> outcome.goalName <> "\n"
        <> "  " <> tshow (length proofs) <> " proof(s) found:\n"
        <> formatProofsForLLM proofs
        <> "\n  " <> tshow (length blocks) <> " question(s) blocking further exploration:\n"
        <> T.unlines ["    - " <> b.question | b <- blocks]
  where
    formatProofsForLLM :: [SolverSuccess] -> Text
    formatProofsForLLM ps = T.unlines (zipWith formatOneProof [1 :: Int ..] ps)

    formatOneProof :: Int -> SolverSuccess -> Text
    formatOneProof i s =
      "    Proof " <> tshow i <> ":\n"
        <> T.unlines (map ("      " <>) (T.lines (dispText s)))

    formatFailuresForLLM :: [FailurePath] -> Text
    formatFailuresForLLM [] = ""
    formatFailuresForLLM fs =
      "  Failure paths:\n"
        <> T.unlines ["    - " <> fp.ruleName <> ": " <> fp.reason | fp <- fs]

-- | De-duplicate blocks by (inputType, name, arguments).
dedupBlocks :: [UserInputBlock] -> [UserInputBlock]
dedupBlocks = go []
  where
    go _ [] = []
    go seen (b : bs)
      | key b `elem` seen = go seen bs
      | otherwise = b : go (key b : seen) bs
    key b = (b.inputType, b.name, b.arguments)

tshow :: (Show a) => a -> Text
tshow = fromString . show
