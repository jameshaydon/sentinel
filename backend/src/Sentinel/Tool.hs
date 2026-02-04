-- | Tool definitions for the Sentinel framework.
--
-- This module defines:
-- - 'Tool': A self-contained tool with execution logic, guards, and fact production
-- - 'ToolGuard': Declarative solver guards for tool preconditions
-- - 'LLMTool': Tool metadata for the LLM (name, description, params)
-- - 'ToolOutput': The result of tool execution (observation + produced facts)
-- - 'ToolCategory': Whether a tool is a data tool (can be auto-invoked) or action tool
-- - 'SideSessionSpec': Specification for triggering a side conversation session
--
-- All guards use the declarative 'SolverGuard' system, which provides full proof
-- traces and supports blocking on context or askables.
module Sentinel.Tool
  ( -- * Tool Categories
    ToolCategory (..),

    -- * Tool Output
    ToolOutput (..),

    -- * Side Session Specification
    SideSessionSpec (..),

    -- * Tool Guards
    Guard,
    ToolGuard (..),

    -- * Tool Definition
    Tool (..),

    -- * Query Definition
    Query (..),
    queryToLLMTool,

    -- * LLM Tool Metadata
    LLMTool (..),
    toLLMTool,
    toOpenAITool,
  )
where

import Data.Aeson (Value)
import OpenAI.V1.Tool qualified as OpenAI
import Pre
import Sentinel.Context (ContextDecl)
import Sentinel.Sentinel (SentinelM)
import Sentinel.Solver.Askable (AskableDecl)
import Sentinel.Solver.Combinators (SolverM)
import Sentinel.Solver.Types (BaseFact, Proof, Scalar, SolverOutcome)

--------------------------------------------------------------------------------
-- Tool Categories
--------------------------------------------------------------------------------

-- | Tool category determines automatic invocation behavior.
data ToolCategory
  = -- | Can be auto-invoked by guards for fact resolution
    DataTool
  | -- | Requires explicit LLM invocation, may have side effects
    ActionTool
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Side Session Specification
--------------------------------------------------------------------------------

-- | Specification for triggering a side conversation session.
--
-- When an Ask tool is called, it returns a SideSessionSpec that triggers
-- a synchronous side conversation with the user. The side conversation
-- uses only the Set/Confirm/Deny tools appropriate to the type.
data SideSessionSpec
  = -- | Side session for establishing a context variable value
    ContextSession Text ContextDecl -- (name, decl)
  | -- | Side session for confirming/denying an askable fact
    AskableSession Text AskableDecl [Scalar] -- (name, decl, args)
  deriving stock (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Tool Output
--------------------------------------------------------------------------------

-- | Output from tool execution.
--
-- Combines the observation text (returned to the LLM) with any facts
-- produced by the tool execution (added to the fact store as BaseFacts).
-- Optionally triggers a side session for user interaction.
data ToolOutput = ToolOutput
  { observation :: Text,
    producedFacts :: [BaseFact],
    -- | If set, triggers a synchronous side conversation session
    triggerSideSession :: Maybe SideSessionSpec,
    -- | Solver outcome from the tool (if the tool ran the solver internally)
    solverOutcome :: Maybe SolverOutcome
  }
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------
-- Tool Guards
--------------------------------------------------------------------------------

-- | A guard is a function from tool arguments to a solver action that produces
-- a proof (or fails/blocks).
--
-- This is the same pattern as rules: @brit :: Person -> M Proof@
type Guard = Value -> SolverM Proof

-- | Guard types for tools.
--
-- All guards use the declarative solver system for consistent proof traces.
data ToolGuard
  = -- | Declarative guard using the solver.
    --
    -- The Text is the guard name (for audit/logging).
    -- The Guard function takes tool args and produces a proof or fails/blocks.
    SolverGuardT Text Guard
  | -- | No guard - tool always allowed.
    NoGuard

--------------------------------------------------------------------------------
-- Tool Definition
--------------------------------------------------------------------------------

-- | A self-contained tool definition.
--
-- Each tool bundles:
-- - Metadata for the LLM (name, description, params)
-- - Category (data vs action)
-- - Guard (precondition for tool execution)
-- - Execute (implementation that returns observation + facts)
--
-- Type parameter:
-- - @db@: The database type (e.g., AirlineDB)
data Tool db = Tool
  { -- | Tool name (used for lookup and LLM tool calling)
    name :: Text,
    -- | Description for the LLM
    description :: Text,
    -- | JSON Schema for input parameters
    params :: Value,
    -- | Tool category (data or action)
    category :: ToolCategory,
    -- | Guard for tool execution (precondition that must pass).
    guard :: ToolGuard,
    -- | Execution function (returns observation + produced facts)
    execute :: Value -> ExceptT Text (SentinelM db) ToolOutput
  }

--------------------------------------------------------------------------------
-- Query Definition
--------------------------------------------------------------------------------

-- | A first-class query: runs the solver to evaluate a predicate and reports
-- the results (proofs + blocks) directly to the LLM. Unlike a guard, a query
-- does not gate tool execution — it just reports what the solver finds.
--
-- Not parameterized by @db@ because the goal is pure solver logic.
data Query = Query
  { -- | Query name (used for lookup and LLM tool calling)
    name :: Text,
    -- | Description for the LLM
    description :: Text,
    -- | JSON Schema for input parameters
    params :: Value,
    -- | The solver goal to evaluate
    goal :: Guard -- Value -> SolverM Proof
  }

-- | Convert a Query to LLM-facing tool metadata.
queryToLLMTool :: Query -> LLMTool
queryToLLMTool query =
  LLMTool
    { name = query.name,
      description = query.description,
      params = query.params
    }

--------------------------------------------------------------------------------
-- LLM Tool Metadata
--------------------------------------------------------------------------------

-- | Tool metadata for the LLM.
--
-- This is what the Agent passes to the LLM for tool calling.
-- Contains only schema information - no guards or execution logic.
data LLMTool = LLMTool
  { name :: Text,
    description :: Text,
    params :: Value
  }
  deriving stock (Generic)

-- | Extract LLM-facing metadata from a full Tool definition.
toLLMTool :: Tool db -> LLMTool
toLLMTool tool =
  LLMTool
    { name = tool.name,
      description = tool.description,
      params = tool.params
    }

-- | Convert to OpenAI tool format.
toOpenAITool :: LLMTool -> OpenAI.Tool
toOpenAITool tool =
  OpenAI.Tool_Function
    { function =
        OpenAI.Function
          { name = tool.name,
            description = Just tool.description,
            parameters = Just tool.params,
            strict = Nothing
          }
    }
