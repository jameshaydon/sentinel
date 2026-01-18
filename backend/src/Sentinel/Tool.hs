-- | Tool metadata for the LLM.
--
-- Tools are defined as simple metadata records containing name, description,
-- and parameters. The Agent passes these to the LLM for tool calling.
-- Actual tool execution is handled by Sentinel.
module Sentinel.Tool
  ( Tool (..),
    toOpenAITool,
  )
where

import Data.Aeson (Value)
import OpenAI.V1.Tool qualified as OpenAI
import Pre

-- | Tool metadata for the LLM.
--
-- This is what the Agent passes to the LLM for tool calling.
-- Contains only schema information - no guards or execution logic.
data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    -- | Full JSON Schema for arguments
    toolParams :: Value
  }
  deriving stock (Generic)

-- | Convert to OpenAI tool format.
toOpenAITool :: Tool -> OpenAI.Tool
toOpenAITool tool =
  OpenAI.Tool_Function
    { function =
        OpenAI.Function
          { name = tool.toolName,
            description = Just tool.toolDescription,
            parameters = Just tool.toolParams,
            strict = Nothing
          }
    }
