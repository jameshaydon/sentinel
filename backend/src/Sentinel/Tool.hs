-- | Representation of tools. Tools are monadic actions that can have effects.
module Sentinel.Tool
  ( Tool (..),
    Toolkit (..),
    toOpenAITool,
  )
where

import Data.Aeson (Value)
import OpenAI.V1.Tool qualified as OpenAI
import Pre

-- | A tool bundles its schema with its execution logic.
-- 'm' is the monadic context (e.g., AgentM db).
data Tool m = Tool
  { toolName :: Text,
    toolDescription :: Text,
    -- | Full JSON Schema for arguments
    toolParams :: Value,
    -- | Logic: takes JSON args, performs effects. Throws on error.
    toolAction :: Value -> ExceptT Text m Text
  }

data Toolkit m = Toolkit
  { tools :: [Tool m],
    systemPrompt :: Text
  }

-- | Convert to OpenAI format using the tool's custom parameters
toOpenAITool :: Tool m -> OpenAI.Tool
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
