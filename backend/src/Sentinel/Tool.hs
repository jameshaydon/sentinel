-- | Representation of tools. Tools are currently just `Text -> Text`, but can
-- also have DB side-effects.
module Sentinel.Tool
  ( Tool (..),
    ToolResult (..),
    Toolkit (..),
    toOpenAITool,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import OpenAI.V1.Tool qualified as OpenAI
import Pre

-- | A tool that the agent can use.
data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolParameterDesc :: Text
  }
  deriving stock (Show, Eq)

-- | Convert a Tool to OpenAI's Tool_Function format.
toOpenAITool :: Tool -> OpenAI.Tool
toOpenAITool tool =
  OpenAI.Tool_Function
    { function =
        OpenAI.Function
          { name = tool.toolName,
            description = Just tool.toolDescription,
            parameters =
              Just
                $ Aeson.Object
                $ KeyMap.fromList
                  [ ("type", "object"),
                    ( "properties",
                      Aeson.Object
                        $ KeyMap.fromList
                          [ ( "input",
                              Aeson.Object
                                $ KeyMap.fromList
                                  [ ("type", "string"),
                                    ("description", Aeson.String tool.toolParameterDesc)
                                  ]
                            )
                          ]
                    ),
                    ("required", Aeson.Array $ pure "input"),
                    ("additionalProperties", Aeson.Bool False)
                  ],
            strict = Just True
          }
    }

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
    -- | Execute a tool. First arg is the tool name. Second is the tool input.
    -- Returns the (text) result and the (potentially modified) database.
    executeTool :: Text -> Text -> db -> (ToolResult, db),
    systemPrompt :: Text
  }
