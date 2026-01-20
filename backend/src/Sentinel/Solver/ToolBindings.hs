-- | Tool bindings for the Sentinel solver.
--
-- Tool bindings describe how solver predicates are established via tool calls.
-- When the solver needs to prove a predicate and it's not already in the fact
-- store, it looks up the tool binding and invokes the corresponding data tool.
--
-- For example, to prove @flight_status("AL-445", Status)@:
-- 1. Look up the binding for @flight_status@
-- 2. Build tool input from the ground argument: @{"flight_id": "AL-445"}@
-- 3. Invoke @get_flight_details@ tool
-- 4. Extract facts from the result: @flight_status("AL-445", "delayed")@
module Sentinel.Solver.ToolBindings
  ( -- * Tool Binding
    ToolBinding (..),

    -- * Arg Builders
    singleArgBuilder,

    -- * Tool Binding Registry
    ToolBindingRegistry (..),
    emptyToolBindingRegistry,
    registerBinding,
    lookupBinding,
    allBindings,
  )
where

import Data.Aeson (Value, object)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Pre
import Sentinel.Solver.Types (Scalar, scalarToJSON)

--------------------------------------------------------------------------------
-- Tool Binding
--------------------------------------------------------------------------------

-- | A binding describes how a predicate is established via tool call.
--
-- When the solver encounters a predicate that isn't in the fact store, it
-- uses the binding to:
-- 1. Build tool input JSON from the ground (input) arguments
-- 2. Invoke the named data tool
-- 3. Facts are produced by the Tool's execute function via ToolOutput.producedFacts
--
-- === Input vs Output Arguments
--
-- Predicates have input and output argument positions:
--
-- - @flight_status(FlightId, Status)@ has FlightId as input, Status as output
-- - To query the predicate, FlightId must be ground (known)
-- - Status is unified with the value extracted from the tool result
--
-- The 'inputArity' field specifies how many arguments are inputs.
-- The 'buildArgs' function receives exactly that many ground 'Scalar' values.
--
-- === Example
--
-- @
-- flightStatusBinding = ToolBinding
--   { predicate = "flight_status"
--   , inputArity = 1
--   , toolName = "CheckFlightStatus"
--   , description = "Get flight status from flight number"
--   , buildArgs = \\[flightId] ->
--       object ["flightNumber" .= scalarToJSON flightId]
--   }
-- @
data ToolBinding = ToolBinding
  { -- | The predicate name this binding is for (e.g., "flight_status")
    predicate :: Text,
    -- | Number of input arguments required to invoke the tool.
    -- These must be ground (bound to concrete values) before calling.
    inputArity :: Int,
    -- | The name of the data tool to invoke (e.g., "CheckFlightStatus")
    toolName :: Text,
    -- | Human-readable description of what this binding does
    description :: Text,
    -- | Build the JSON input for the tool from ground arguments.
    -- Receives exactly 'inputArity' 'Scalar' values.
    buildArgs :: [Scalar] -> Value
  }

instance Show ToolBinding where
  show b =
    "ToolBinding { predicate = "
      <> show b.predicate
      <> ", inputArity = "
      <> show b.inputArity
      <> ", toolName = "
      <> show b.toolName
      <> " }"

--------------------------------------------------------------------------------
-- Arg Builders
--------------------------------------------------------------------------------

-- | Build a JSON object with a single field from exactly one input argument.
--
-- This helper reduces boilerplate for the common case where a tool binding
-- expects a single input argument and wraps it in a JSON object.
--
-- === Example
--
-- @
-- buildArgs = singleArgBuilder "flightNumber" "flight_status"
-- -- Equivalent to:
-- buildArgs = \\case
--   [flightId] -> object ["flightNumber" .= scalarToJSON flightId]
--   args -> error $ "flight_status expects 1 input arg, got " <> show (length args)
-- @
singleArgBuilder ::
  -- | JSON field name for the argument
  Text ->
  -- | Predicate name (for error messages)
  Text ->
  -- | Function from input args to JSON
  ([Scalar] -> Value)
singleArgBuilder fieldName predName = \case
  [arg] -> object [fromString (T.unpack fieldName) Aeson..= scalarToJSON arg]
  args -> error $ T.unpack predName <> " expects 1 input arg, got " <> show (length args)

--------------------------------------------------------------------------------
-- Tool Binding Registry
--------------------------------------------------------------------------------

-- | Registry of tool bindings, keyed by predicate name.
--
-- The solver uses this registry to look up how to establish predicates.
-- Each predicate can have at most one binding.
newtype ToolBindingRegistry = ToolBindingRegistry
  { bindings :: Map Text ToolBinding
  }
  deriving stock (Show)

-- | Empty tool binding registry.
emptyToolBindingRegistry :: ToolBindingRegistry
emptyToolBindingRegistry = ToolBindingRegistry M.empty

-- | Register a tool binding.
--
-- If a binding for the predicate already exists, it is replaced.
registerBinding :: ToolBinding -> ToolBindingRegistry -> ToolBindingRegistry
registerBinding binding (ToolBindingRegistry bs) =
  ToolBindingRegistry (M.insert binding.predicate binding bs)

-- | Look up a tool binding by predicate name.
lookupBinding :: Text -> ToolBindingRegistry -> Maybe ToolBinding
lookupBinding predName (ToolBindingRegistry bs) = M.lookup predName bs

-- | Get all registered bindings.
allBindings :: ToolBindingRegistry -> [ToolBinding]
allBindings (ToolBindingRegistry bs) = M.elems bs
