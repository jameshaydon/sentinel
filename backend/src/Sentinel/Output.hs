-- | Display formatting
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
    nest 4
      $ vsep
        [ "",
          subheader "## Thinking",
          "",
          thinking (wrappedText thought)
        ]

-- | Agent's final answer to the user
data FinalAnswer = FinalAnswer {answer :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp FinalAnswer where
  disp (FinalAnswer answer) =
    nest 4
      $ vsep
        [ "",
          subheader "## Final Answer",
          "",
          finalAnswer (wrappedText answer)
        ]

-- | Agent using a tool
data ToolUse = ToolUse
  { toolName :: Text,
    toolInput :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp ToolUse where
  disp (ToolUse tool input) =
    nest 4
      $ vsep
        [ "",
          subheader "## Tool Use",
          "",
          label "Tool:" <+> styledToolName (pretty tool),
          label "Input:" <+> dimText (wrappedText input)
        ]

-- | Result/observation from a tool execution
data Observation = Observation {result :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Observation where
  disp (Observation result) =
    nest 4
      $ vsep
        [ "",
          subheader "## Observation",
          "",
          styledObservation (wrappedText result)
        ]

-- | Conversational response from the agent (not in ReAct format)
data Response = Response {message :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Response where
  disp (Response resp) =
    nest 4
      $ vsep
        [ "",
          subheader "## Response",
          "",
          wrappedText resp
        ]

-- | Error message
data Error = Error {errorMessage :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Error where
  disp (Error err) =
    nest 4
      $ vsep
        [ "",
          errorText "## Error",
          "",
          errorText (label "LLM Error:" <+> wrappedText err)
        ]

-- | Iteration header/marker
data Iteration = Iteration {iterationNumber :: Int}
  deriving stock (Show, Eq, Generic)

instance Disp Iteration where
  disp (Iteration n) =
    nest 2
      $ vsep
        [ "",
          header "# Iteration" <+> iterationNum (pretty n)
        ]

-- | Agent start marker with user query
data AgentStart = AgentStart {userQuery :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp AgentStart where
  disp (AgentStart query) =
    vsep
      [ "",
        header "# Agent Start",
        "",
        label "User Query:" <+> userText (wrappedText query)
      ]
