-- | Askable predicates for the Sentinel solver.
--
-- Askable predicates are facts that can only be established by asking the user.
-- They represent information that cannot be looked up from tools but must be
-- confirmed by the user (e.g., "Do you understand your booking will be cancelled?").
--
-- When the solver encounters an askable predicate that hasn't been established,
-- it blocks and returns a question for the LLM to ask the user.
module Sentinel.Solver.Askable
  ( -- * Askable Declaration
    AskableDecl (..),
    EvidenceType (..),

    -- * Askable Registry
    AskableRegistry (..),
    emptyAskableRegistry,
    declareAskable,
    lookupAskable,

    -- * Question Formatting
    formatQuestion,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as Text
import Pre
import Sentinel.Solver.Types (Scalar (..), ScalarType (..), scalarToText)

--------------------------------------------------------------------------------
-- Askable Declaration
--------------------------------------------------------------------------------

-- | An askable predicate declaration.
--
-- Askable predicates are facts that must be established by user confirmation.
-- Each declaration includes:
-- - The predicate name and typed arguments
-- - A template for the question to ask
-- - What kind of evidence establishes this fact
data AskableDecl = AskableDecl
  { -- | The predicate name (e.g., "user_confirms_cancellation")
    predicate :: Text,
    -- | Types of each argument (e.g., [TextType] for user_confirms_cancellation(User))
    argumentTypes :: [ScalarType],
    -- | Template for the question to ask the user.
    -- Use {0}, {1}, etc. for argument placeholders.
    questionTemplate :: Text,
    -- | What kind of evidence establishes this predicate
    evidenceType :: EvidenceType,
    -- | Human-readable description for the LLM
    description :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Types of evidence that can establish an askable predicate.
data EvidenceType
  = -- | User made a statement in conversation (e.g., said "yes")
    UserStatement
  | -- | User explicitly clicked a confirmation button
    ExplicitConfirmation
  | -- | User uploaded a specific type of document
    DocumentUpload Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Askable Registry
--------------------------------------------------------------------------------

-- | Registry of askable predicate declarations.
newtype AskableRegistry = AskableRegistry
  { askables :: Map Text AskableDecl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty askable registry.
emptyAskableRegistry :: AskableRegistry
emptyAskableRegistry = AskableRegistry M.empty

-- | Declare an askable predicate.
declareAskable :: AskableDecl -> AskableRegistry -> AskableRegistry
declareAskable decl (AskableRegistry as) =
  AskableRegistry (M.insert decl.predicate decl as)

-- | Look up an askable declaration by predicate name.
lookupAskable :: Text -> AskableRegistry -> Maybe AskableDecl
lookupAskable askName (AskableRegistry as) = M.lookup askName as

--------------------------------------------------------------------------------
-- Question Formatting
--------------------------------------------------------------------------------

-- | Format a question template with scalar arguments.
--
-- Replaces {0}, {1}, etc. with the corresponding argument values.
-- For example: "Can you provide a {0}?" with args [ScStr "death certificate"]
-- becomes "Can you provide a death certificate?"
formatQuestion :: Text -> [Scalar] -> Text
formatQuestion template args = foldl' replaceArg template (zip [0 :: Int ..] args)
  where
    replaceArg :: Text -> (Int, Scalar) -> Text
    replaceArg t (i, arg) =
      let placeholder = "{" <> Text.pack (show i) <> "}"
          replacement = scalarToText arg
       in Text.replace placeholder replacement t
