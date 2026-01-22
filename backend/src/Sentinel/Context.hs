-- | Context management for the Sentinel solver.
--
-- Context variables are named slots that hold values relevant to the current
-- conversation. For example, "booking_of_interest" might hold the booking ID
-- that the user is asking about.
--
-- Context variables can be:
-- - Pre-seeded from session data (e.g., "current_user" from authentication)
-- - Established by user selection from candidates
-- - Derived from other context (e.g., "flight_of_interest" from booking)
module Sentinel.Context
  ( -- * Context Store
    ContextStore (..),
    emptyContextStore,
    getContext,
    setContext,

    -- * Context Establishment
    ContextEstablishment (..),
    EstablishmentMethod (..),

    -- * Context Declarations
    ContextDecl (..),
    ContextDecls (..),
    SeedSpec (..),
    AskableSpec (..),
    emptyContextDecls,
    declareContext,
    lookupContextDecl,
  )
where

import Data.Map.Strict qualified as M
import Data.Time (UTCTime)
import Pre
import Sentinel.Solver.Types (Scalar (..), ScalarType (..))

--------------------------------------------------------------------------------
-- Context Store
--------------------------------------------------------------------------------

-- | The context store holds established context values.
--
-- Each slot either has a value (with establishment metadata) or is empty.
data ContextStore = ContextStore
  { -- | Established context values with metadata
    established :: Map Text ContextEstablishment
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | How a context value was established.
data ContextEstablishment = ContextEstablishment
  { -- | The actual value
    value :: Scalar,
    -- | How the value was established
    establishedVia :: EstablishmentMethod,
    -- | When the value was established
    timestamp :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Methods by which context can be established.
data EstablishmentMethod
  = -- | User selected from a list of candidates
    UserSelection [Scalar]
  | -- | Pre-seeded from session data (e.g., authentication)
    SystemSeeded
  | -- | Derived from another context value
    DerivedFrom Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create an empty context store.
emptyContextStore :: ContextStore
emptyContextStore = ContextStore M.empty

-- | Get the value of a context variable, if established.
getContext :: Text -> ContextStore -> Maybe Scalar
getContext slot store = (.value) <$> M.lookup slot store.established

-- | Set a context variable with establishment metadata.
setContext :: Text -> ContextEstablishment -> ContextStore -> ContextStore
setContext slot establishment store =
  store {established = M.insert slot establishment store.established}

--------------------------------------------------------------------------------
-- Context Declarations
--------------------------------------------------------------------------------

-- | A context variable declaration.
--
-- Declarations specify:
-- - The name of the context slot
-- - The type of the value
-- - Optionally, a seed specification for pre-seeding from session
-- - Optionally, an askable spec for asking the user
data ContextDecl = ContextDecl
  { -- | The name of the context slot (e.g., "booking_of_interest")
    name :: Text,
    -- | Type of the context value (for schema validation)
    valueType :: ScalarType,
    -- | How to pre-seed this context from session data
    seedValue :: Maybe SeedSpec,
    -- | If Just, this context can be established by asking the user
    -- If Nothing, it can only be pre-seeded
    askable :: Maybe AskableSpec,
    -- | Human-readable description for the LLM
    description :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Specification for how to ask the user for a context value.
data AskableSpec = AskableSpec
  { -- | Template for the question to ask (e.g., "Which booking are you asking about?")
    questionTemplate :: Text,
    -- | Explicit candidate values (can be empty for open questions)
    -- The LLM can also provide candidates dynamically when calling the Ask tool
    candidates :: [Scalar]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Specification for pre-seeding context from session data.
data SeedSpec
  = -- | Seed from a named session field (e.g., "user_id")
    FromSession Text
  | -- | Seed with a constant value
    Constant Scalar
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Registry of context variable declarations.
newtype ContextDecls = ContextDecls
  { decls :: Map Text ContextDecl
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty context declarations.
emptyContextDecls :: ContextDecls
emptyContextDecls = ContextDecls M.empty

-- | Declare a context variable.
declareContext :: ContextDecl -> ContextDecls -> ContextDecls
declareContext decl (ContextDecls ds) =
  ContextDecls (M.insert decl.name decl ds)

-- | Look up a context declaration by name.
lookupContextDecl :: Text -> ContextDecls -> Maybe ContextDecl
lookupContextDecl ctxName (ContextDecls ds) = M.lookup ctxName ds
