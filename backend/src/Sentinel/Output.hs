module Sentinel.Output
  ( -- * Output Types
    Thinking (..),
    FinalAnswer (..),
    ToolUse (..),
    Observation (..),
    Response (..),
    Error (..),
    Iteration (..),
    AgentStart (..),
  )
where

import Pre

-- | Agent's internal reasoning/thinking
data Thinking = Thinking {thought :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Thinking where
  disp (Thinking thought) =
    nest 4 $
      vsep
        [ "",
          boldText "## Thinking",
          "",
          wrappedText thought
        ]

-- | Agent's final answer to the user
data FinalAnswer = FinalAnswer {answer :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp FinalAnswer where
  disp (FinalAnswer answer) =
    nest 4 $
      vsep
        [ "",
          boldText "## Final Answer",
          "",
          wrappedText answer
        ]

-- | Agent using a tool
data ToolUse = ToolUse
  { toolName :: Text,
    toolInput :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp ToolUse where
  disp (ToolUse tool input) =
    nest 4 $
      vsep
        [ "",
          boldText "## Tool Use",
          "",
          boldText "Tool:" <+> pretty tool,
          boldText "Input:" <+> wrappedText input
        ]

-- | Result/observation from a tool execution
data Observation = Observation {result :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Observation where
  disp (Observation result) =
    nest 4 $
      vsep
        [ "",
          boldText "## Result",
          "",
          wrappedText result
        ]

-- | Conversational response from the agent (not in ReAct format)
data Response = Response {message :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Response where
  disp (Response resp) =
    nest 4 $
      vsep
        [ "",
          boldText "## Response",
          "",
          wrappedText resp
        ]

-- | Error message
data Error = Error {errorMessage :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Error where
  disp (Error err) =
    nest 4 $
      vsep
        [ "",
          boldText "## Error",
          "",
          "LLM Error:" <+> wrappedText err
        ]

-- | Iteration header/marker
data Iteration = Iteration {iterationNumber :: Int}
  deriving stock (Show, Eq, Generic)

instance Disp Iteration where
  disp (Iteration n) =
    nest 2 $
      vsep
        [ "",
          boldText ("# Iteration" <+> pretty n)
        ]

-- | Agent start marker with user query
data AgentStart = AgentStart {userQuery :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp AgentStart where
  disp (AgentStart query) =
    vsep
      [ "",
        boldText "# Agent Start",
        "",
        "User Query:" <+> wrappedText query
      ]
