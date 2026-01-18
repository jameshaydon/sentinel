-- | Tool definitions for the Sentinel framework.
--
-- This module defines:
-- - 'Tool': A self-contained tool with execution logic, guards, and fact production
-- - 'LLMTool': Tool metadata for the LLM (name, description, params)
-- - 'ToolOutput': The result of tool execution (observation + produced facts)
-- - 'ToolCategory': Whether a tool is a data tool (can be auto-invoked) or action tool
module Sentinel.Tool
  ( -- * Tool Categories
    ToolCategory (..),

    -- * Tool Output
    ToolOutput (..),

    -- * Tool Definition
    Tool (..),

    -- * LLM Tool Metadata
    LLMTool (..),
    toLLMTool,
    toOpenAITool,
  )
where

import Data.Aeson (Value)
import OpenAI.V1.Tool qualified as OpenAI
import Pre
import Sentinel.Guard (GuardM)
import Sentinel.Sentinel (SentinelM)

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
-- Tool Output
--------------------------------------------------------------------------------

-- | Output from tool execution.
--
-- Combines the observation text (returned to the LLM) with any facts
-- produced by the tool execution (added to the fact store).
data ToolOutput fact = ToolOutput
  { observation :: Text,
    producedFacts :: [fact]
  }
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------
-- Tool Definition
--------------------------------------------------------------------------------

-- | A self-contained tool definition.
--
-- Each tool bundles:
-- - Metadata for the LLM (name, description, params)
-- - Category (data vs action)
-- - Guard (precondition that can invoke data tools)
-- - Execute (implementation that returns observation + facts)
--
-- Type parameters:
-- - @db@: The database type (e.g., AirlineDB)
-- - @fact@: The fact type (e.g., AirCanada.Fact)
data Tool db fact = Tool
  { -- | Tool name (used for lookup and LLM tool calling)
    name :: Text,
    -- | Description for the LLM
    description :: Text,
    -- | JSON Schema for input parameters
    params :: Value,
    -- | Tool category (data or action)
    category :: ToolCategory,
    -- | Guard function (precondition for tool execution)
    guard :: Value -> GuardM db fact (),
    -- | Execution function (returns observation + produced facts)
    execute :: Value -> ExceptT Text (SentinelM db fact) (ToolOutput fact)
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
toLLMTool :: Tool db fact -> LLMTool
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
