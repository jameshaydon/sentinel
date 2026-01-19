-- | Solver-backed query tools for the Air Canada example.
--
-- These tools allow the LLM agent to:
-- - Query refund eligibility (informational, using the solver)
-- - Establish context variables (e.g., which booking the user is asking about)
-- - Record user confirmations for askable predicates
--
-- Together with the standard data/action tools, these form the complete
-- interface for the agent to interact with the Sentinel solver.
module Examples.AirCanada.Queries
  ( -- * Query Tools
    queryEligibilityTool,
    establishContextTool,
    establishAskableTool,

    -- * All Query Tools
    airCanadaQueryTools,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Pre
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel qualified as Sentinel
import Sentinel.Solver.Types (Scalar (..))
import Sentinel.Tool (Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))

--------------------------------------------------------------------------------
-- Tool Collection
--------------------------------------------------------------------------------

-- | All query tools for Air Canada.
--
-- These tools are added to the toolkit alongside the standard data/action tools.
airCanadaQueryTools :: [Tool db]
airCanadaQueryTools =
  [ queryEligibilityTool,
    establishContextTool,
    establishAskableTool
  ]

--------------------------------------------------------------------------------
-- Query Eligibility Tool
--------------------------------------------------------------------------------

-- | Tool for querying refund eligibility.
--
-- This is an informational query tool. The LLM uses it to check what
-- refund options are available before attempting to process a refund.
--
-- The tool invokes the solver with the @eligibleForRefund@ rule and returns:
-- - Success: The available refund options with reasons and proof
-- - BlockedOnContext: Which context needs to be established
-- - BlockedOnAskable: Which user confirmation is needed
-- - Failure: Why no refund is available
--
-- Note: This tool is a placeholder that describes the intended functionality.
-- The actual solver integration is handled by the Agent, which manages the
-- SolverEnv and runs the solver.
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
        --
        -- In practice, the Agent would:
        -- 1. Detect this tool call
        -- 2. Run the solver with eligibleForRefund
        -- 3. Interpret the SolverResult and respond appropriately
        --
        -- For now, we return a message indicating the tool was called.
        pure
          ToolOutput
            { observation =
                "QueryRefundEligibility requires solver integration. "
                  <> "The Agent should run the eligibleForRefund rule via the solver.",
              producedFacts = []
            }
    }

--------------------------------------------------------------------------------
-- Establish Context Tool
--------------------------------------------------------------------------------

-- | Tool for establishing a context variable.
--
-- When the solver blocks on a context variable (e.g., @booking_of_interest@),
-- the LLM asks the user to select a value and then calls this tool to
-- establish it.
--
-- Arguments:
-- - @slot@: The context variable name (e.g., "booking_of_interest")
-- - @value@: The value to set
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
        lift $ Sentinel.setContextValue slot scalarValue []

        pure
          ToolOutput
            { observation = "Context established: " <> slot <> " = " <> value,
              producedFacts = []
            }
    }

--------------------------------------------------------------------------------
-- Establish Askable Tool
--------------------------------------------------------------------------------

-- | Tool for recording user confirmation of an askable predicate.
--
-- When the solver blocks on an askable predicate (e.g., @user_claims_bereavement@),
-- the LLM asks the user the corresponding question and then calls this tool
-- to record their response.
--
-- Arguments:
-- - @predicate@: The askable predicate name
-- - @arguments@: JSON array of scalar arguments
-- - @confirmed@: Whether the user confirmed (true) or denied (false)
establishAskableTool :: Tool db
establishAskableTool =
  Tool
    { name = "EstablishAskable",
      description =
        "Record the user's response to an askable predicate. "
          <> "Use this after asking the user a confirmation question. "
          <> "Example: EstablishAskable(predicate='user_claims_bereavement', "
          <> "arguments=['current_user'], confirmed=true)",
      params =
        Schema.objectSchema
          [ ("predicate", Schema.stringProp "The askable predicate name"),
            ("arguments", Schema.stringProp "JSON array of arguments (as strings)"),
            ("confirmed", Schema.enumProp ["true", "false"] "Whether the user confirmed")
          ]
          ["predicate", "confirmed"],
      category = ActionTool, -- Recording confirmations is an action
      guard = NoGuard,
      execute = \args -> do
        predicate <- extractString "predicate" args ??: "Missing 'predicate' parameter"
        confirmedStr <- extractString "confirmed" args ??: "Missing 'confirmed' parameter"
        let confirmed = confirmedStr == "true"

        -- Parse arguments (optional, defaults to empty)
        let arguments = parseArguments (extractString "arguments" args)

        -- Record the askable fact in the Sentinel's askable fact store
        lift $ Sentinel.setAskableFact predicate arguments confirmed

        pure
          ToolOutput
            { observation =
                "Askable fact recorded: "
                  <> predicate
                  <> "("
                  <> showArgs arguments
                  <> ") = "
                  <> (if confirmed then "confirmed" else "denied"),
              producedFacts = []
            }
    }
  where
    -- Parse a JSON array of strings into Scalars
    parseArguments :: Maybe Text -> [Scalar]
    parseArguments Nothing = []
    parseArguments (Just jsonStr) =
      case Aeson.decode (fromString (T.unpack jsonStr)) of
        Just (Aeson.Array arr) ->
          mapMaybe jsonToScalar (toList arr)
        _ -> []

    jsonToScalar :: Aeson.Value -> Maybe Scalar
    jsonToScalar (Aeson.String s) = Just (ScStr s)
    jsonToScalar (Aeson.Number n) = Just (ScNum (realToFrac n))
    jsonToScalar (Aeson.Bool b) = Just (ScBool b)
    jsonToScalar _ = Nothing

    showArgs :: [Scalar] -> Text
    showArgs [] = ""
    showArgs args = T.intercalate ", " (showScalar <$> args)

    showScalar :: Scalar -> Text
    showScalar (ScStr s) = s
    showScalar (ScNum n) = fromString (show n)
    showScalar (ScBool b) = if b then "true" else "false"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Extract a string value from JSON args.
extractString :: Text -> Aeson.Value -> Maybe Text
extractString key (Aeson.Object obj) =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (Aeson.String s) -> Just s
    _ -> Nothing
extractString _ _ = Nothing
