-- | Display formatting
module Sentinel.Output
  ( -- * Output Types
    Thinking (..),
    FinalAnswer (..),
    ToolUse (..),
    Observation (..),
    ToolError (..),
    Response (..),
    Error (..),
    Iteration (..),
    TurnStart (..),
    GuardPass (..),
    GuardDenied (..),
    ResolutionAttempt (..),
    QueryExecution (..),
    NeedsUserInput (..),
    EligibilityCheckResult (..),
  )
where

import Pre
import Sentinel.Solver.Types (SolverSuccess (..))

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
  disp (FinalAnswer _answer) =
    nest 4
      $ vsep
        [ "",
          subheader "## Final Answer (LLM)"
        ]

-- | Agent (LLM) using a tool
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
          subheader "## Tool Use" <+> dimText "(LLM)",
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
          subheader "## Result",
          "",
          styledObservation (wrappedText result)
        ]

-- | Error from a tool execution
data ToolError = ToolError {toolErrorMessage :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp ToolError where
  disp (ToolError err) =
    nest 4
      $ vsep
        [ "",
          errorText "## Tool Error",
          "",
          errorText (wrappedText err)
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

-- | Turn start marker with turn number and user query
data TurnStart = TurnStart
  { turnNumber :: Int,
    userQuery :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp TurnStart where
  disp (TurnStart turn _query) =
    vsep
      [ "",
        header "# Turn" <+> iterationNum (pretty turn)
      ]

-- | Guard check passed
data GuardPass = GuardPass
  { guardToolName :: Text,
    guardProofs :: NonEmpty SolverSuccess
  }
  deriving stock (Show, Eq, Generic)

instance Disp GuardPass where
  disp (GuardPass tool proofs) =
    let (firstProof :| _) = proofs
     in nest 4
          $ vsep
            [ "",
              successText "✓" <+> label "Guard passed for" <+> styledToolName (pretty tool),
              "",
              indent 2 (disp firstProof)
            ]

-- | Guard check denied
data GuardDenied = GuardDenied
  { deniedToolName :: Text,
    denialReason :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp GuardDenied where
  disp (GuardDenied tool reason) =
    nest 4
      $ vsep
        [ "",
          errorText "✗ Guard denied for" <+> styledToolName (pretty tool),
          "",
          errorText (wrappedText reason)
        ]

-- | Resolution attempt for a blocked guard
data ResolutionAttempt = ResolutionAttempt
  { attemptToolName :: Text,
    attemptNumber :: Int,
    queriesCount :: Int
  }
  deriving stock (Show, Eq, Generic)

instance Disp ResolutionAttempt where
  disp (ResolutionAttempt tool attempt count) =
    nest 4
      $ vsep
        [ "",
          label "⟳ Resolving guard for" <+> styledToolName (pretty tool),
          dimText ("  Attempt " <> pretty attempt <> ", running " <> pretty count <> " query(s)")
        ]

-- | Tool call triggered by Sentinel for data gathering (not LLM-initiated)
data QueryExecution = QueryExecution
  { queryName :: Text,
    queryInput :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp QueryExecution where
  disp (QueryExecution name input) =
    nest 4
      $ vsep
        [ "",
          subheader "## Tool Use" <+> dimText "(Sentinel)",
          "",
          label "Tool:" <+> styledToolName (pretty name),
          label "Input:" <+> dimText (wrappedText input)
        ]

-- | Guard needs user input to proceed
data NeedsUserInput = NeedsUserInput
  { inputToolName :: Text,
    question :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp NeedsUserInput where
  disp (NeedsUserInput tool q) =
    nest 4
      $ vsep
        [ "",
          label "? Guard for" <+> styledToolName (pretty tool) <+> label "needs user input:",
          "",
          wrappedText q
        ]

-- | Result of an eligibility check (from CheckEligibility tool)
data EligibilityCheckResult
  = EligibilityVerified Text (NonEmpty SolverSuccess)
  | EligibilityDenied Text Text
  | EligibilityNeedsInfo Text Text
  deriving stock (Show, Eq, Generic)

instance Disp EligibilityCheckResult where
  disp = \case
    EligibilityVerified claim proofs ->
      let (firstProof :| _) = proofs
       in nest 4
            $ vsep
              [ successText "✓" <+> label "Verified:" <+> pretty claim,
                "",
                indent 2 (disp firstProof)
              ]
    EligibilityDenied claim reason ->
      nest 4
        $ vsep
          [ errorText "✗" <+> label "Not verified:" <+> pretty claim,
            "",
            indent 2 (wrappedText reason)
          ]
    EligibilityNeedsInfo claim question ->
      nest 4
        $ vsep
          [ label "?" <+> label "Needs info for:" <+> pretty claim,
            "",
            indent 2 (wrappedText question)
          ]
