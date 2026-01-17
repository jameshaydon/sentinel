module Sentinel.Tool
  ( Tool (..),
    ToolResult (..),
    Toolkit (..),
    toolsDescription,
  )
where

import Pre

-- | A tool that the agent can use.
data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolParameterDesc :: Text
  }
  deriving stock (Show, Eq)

instance Disp Tool where
  disp t =
    vsep
      [ "Tool:" <+> pretty t.toolName,
        "Description:" <+> pretty t.toolDescription,
        "Parameter:" <+> pretty t.toolParameterDesc
      ]

-- | Result of executing a tool.
data ToolResult
  = ToolSuccess Text
  | ToolError Text
  deriving stock (Show, Eq)

instance Disp ToolResult where
  disp = \case
    ToolSuccess txt -> pretty txt
    ToolError err -> "ERROR:" <+> pretty err

-- | A toolkit bundles tools with their execution logic and system prompt.
-- Parameterized by the database/state type that tools operate on.
data Toolkit db = Toolkit
  { tools :: [Tool],
    executeTool :: Text -> Text -> db -> ToolResult,
    systemPrompt :: Text
  }

-- | Generate a description of tools for the system prompt.
toolsDescription :: [Tool] -> Text
toolsDescription ts =
  renderDocPlain
    $ vsep
    $ punctuate line (fmap disp ts)
