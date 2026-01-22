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
    DirectQuestion (..),

    -- * Askable UX Types
    AskableQuestion (..),
    ContextQuestion (..),
    AskableAssessmentSummary (..),

    -- * Side Session Types
    AskingUser (..),
    SideSessionResult (..),

    -- * Session Info
    SessionInfo (..),
  )
where

import Pre
import Sentinel.Solver.Types (SolverSuccess (..))

-- | Agent's internal reasoning/thinking
data Thinking = Thinking {thought :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Thinking where
  disp (Thinking thought) =
    section 4 (subheader "## Thinking") (thinking (wrappedText thought))

-- | Agent's final answer to the user
data FinalAnswer = FinalAnswer {answer :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp FinalAnswer where
  disp (FinalAnswer _answer) =
    sectionNoBody 4 (subheader "## Final Answer (LLM)")

-- | Agent (LLM) using a tool
data ToolUse = ToolUse
  { toolName :: Text,
    toolInput :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp ToolUse where
  disp (ToolUse tool input) =
    nest 4 $ vsep ["", toolDisplay (subheader "## Tool Use" <+> dimText "(LLM)") tool input]

-- | Result/observation from a tool execution
data Observation = Observation {result :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Observation where
  disp (Observation result) =
    section 4 (subheader "## Result") (styledObservation (wrappedText result))

-- | Error from a tool execution
data ToolError = ToolError {toolErrorMessage :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp ToolError where
  disp (ToolError err) =
    section 4 (errorText "## Tool Error") (errorText (wrappedText err))

-- | Conversational response from the agent (not in ReAct format)
data Response = Response {message :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Response where
  disp (Response resp) =
    section 4 (subheader "## Response") (wrappedText resp)

-- | Error message
data Error = Error {errorMessage :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp Error where
  disp (Error err) =
    section 4 (errorText "## Error") (errorText (label "LLM Error:" <+> wrappedText err))

-- | Iteration header/marker
data Iteration = Iteration {iterationNumber :: Int}
  deriving stock (Show, Eq, Generic)

instance Disp Iteration where
  disp (Iteration n) =
    sectionNoBody 2 (header "# Iteration" <+> iterationNum (pretty n))

-- | Turn start marker with turn number and user query
data TurnStart = TurnStart
  { turnNumber :: Int,
    userQuery :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp TurnStart where
  disp (TurnStart turn _query) =
    sectionNoBody 0 (header "# Turn" <+> iterationNum (pretty turn))

-- | Guard check passed
data GuardPass = GuardPass
  { guardToolName :: Text,
    guardProofs :: NonEmpty SolverSuccess
  }
  deriving stock (Show, Eq, Generic)

instance Disp GuardPass where
  disp (GuardPass tool proofs) =
    let (firstProof :| _) = proofs
        hdr = successText "✓" <+> label "Guard passed for" <+> styledToolName (pretty tool)
     in section 4 hdr (indent 2 (disp firstProof))

-- | Guard check denied
data GuardDenied = GuardDenied
  { deniedToolName :: Text,
    denialReason :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp GuardDenied where
  disp (GuardDenied tool reason) =
    let hdr = errorText "✗ Guard denied for" <+> styledToolName (pretty tool)
     in section 4 hdr (errorText (wrappedText reason))

-- | Resolution attempt for a blocked guard
data ResolutionAttempt = ResolutionAttempt
  { attemptToolName :: Text,
    attemptNumber :: Int,
    queriesCount :: Int
  }
  deriving stock (Show, Eq, Generic)

instance Disp ResolutionAttempt where
  disp (ResolutionAttempt tool attempt count) =
    let hdr = label "⟳ Resolving guard for" <+> styledToolName (pretty tool)
        detail = dimText ("  Attempt " <> pretty attempt <> ", running " <> pretty count <> " query(s)")
     in nest 4 $ vsep ["", hdr, detail]

-- | Tool call triggered by Sentinel for data gathering (not LLM-initiated)
data QueryExecution = QueryExecution
  { queryName :: Text,
    queryInput :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp QueryExecution where
  disp (QueryExecution name input) =
    nest 4 $ vsep ["", toolDisplay (subheader "## Tool Use" <+> dimText "(Sentinel)") name input]

-- | Guard needs user input to proceed
data NeedsUserInput = NeedsUserInput
  { inputToolName :: Text,
    question :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp NeedsUserInput where
  disp (NeedsUserInput tool q) =
    let hdr = label "? Guard for" <+> styledToolName (pretty tool) <+> label "needs user input:"
     in section 4 hdr (wrappedText q)

-- | Result of an eligibility check (from CheckEligibility tool)
data EligibilityCheckResult
  = EligibilityVerified Text (NonEmpty SolverSuccess)
  | EligibilityDenied Text Text
  | EligibilityNeedsInfo Text Text -- ^ (guard name, question text)
  deriving stock (Show, Eq, Generic)

instance Disp EligibilityCheckResult where
  disp = \case
    EligibilityVerified claim proofs ->
      let (firstProof :| _) = proofs
          hdr = successText "✓" <+> label "Verified:" <+> pretty claim
       in section 4 hdr (indent 2 (disp firstProof))
    EligibilityDenied claim reason ->
      let hdr = errorText "✗" <+> label "Not verified:" <+> pretty claim
       in section 4 hdr (indent 2 (wrappedText reason))
    EligibilityNeedsInfo guardName question ->
      let hdr = label "⏸" <+> label "Blocked:" <+> pretty guardName <+> label "needs info"
       in section 4 hdr $
            vsep
              [ dimText "(Call the appropriate Ask tool to prompt the user)",
                "",
                label "Question:" <+> wrappedText question
              ]

-- | Direct question to user (bypasses LLM, displayed immediately)
data DirectQuestion = DirectQuestion {directQuestionText :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp DirectQuestion where
  disp (DirectQuestion q) =
    section 4 (label "❓ Question for you:") (wrappedText q)

--------------------------------------------------------------------------------
-- Askable UX Types
--------------------------------------------------------------------------------

-- | A single askable question being presented to the user.
-- Used when the AskUserAskable tool is called.
data AskableQuestion = AskableQuestion
  { askablePredicate :: Text,
    askableQuestionText :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp AskableQuestion where
  disp (AskableQuestion predName q) =
    section 4 (label "? Asking User" <+> dimText "(askable fact)") $
      vsep
        [ label "Predicate:" <+> pretty predName,
          label "Question:" <+> wrappedText q
        ]

-- | A context variable question being presented to the user.
-- Used when the Ask_<context> tool is called.
data ContextQuestion = ContextQuestion
  { contextSlotName :: Text,
    contextQuestionText :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Disp ContextQuestion where
  disp (ContextQuestion ctxName q) =
    section 4 (label "? Asking User" <+> dimText "(context variable)") $
      vsep
        [ label "Context:" <+> pretty ctxName,
          label "Question:" <+> wrappedText q
        ]

-- | Summary of askable assessment results.
-- Displayed after the assessment side-session completes.
-- Takes: list of questions asked, user response, and list of (predicate, result) pairs.
data AskableAssessmentSummary a b = AskableAssessmentSummary
  { assessedQuestions :: [a], -- PendingAskable or similar
    userResponseText :: Text,
    assessmentResults :: [(Text, [b], Maybe Bool)] -- (predicate, args, confirmed/denied/ambiguous)
  }
  deriving stock (Show, Eq, Generic)

-- | Helper to display assessment result status
assessmentStatus :: Maybe Bool -> Doc Ann
assessmentStatus = \case
  Just True -> successText "confirmed"
  Just False -> errorText "denied"
  Nothing -> dimText "ambiguous"

instance Disp (AskableAssessmentSummary a b) where
  disp summary =
    section
      4
      (subheader "## Side-Conversation Assessment Complete" <+> dimText "(askable facts established)")
      $ vsep
        [ label "User said:" <+> "\"" <> wrappedText summary.userResponseText <> "\"",
          "",
          label "Facts established:",
          indent 2 $
            vsep
              [ label "•" <+> pretty predName <> ":" <+> assessmentStatus mConfirmed
              | (predName, _args, mConfirmed) <- summary.assessmentResults
              ]
        ]

--------------------------------------------------------------------------------
-- Side Session Types
--------------------------------------------------------------------------------

-- | Display exact question to user during side session.
-- This is shown immediately when an Ask tool triggers a side conversation.
data AskingUser = AskingUser {askingUserQuestion :: Text}
  deriving stock (Show, Eq, Generic)

instance Disp AskingUser where
  disp (AskingUser q) =
    section 4 (label "❓ Question for you:") (wrappedText q)

-- | Result of a side session (displayed after user responds).
data SideSessionResult
  = -- | Success: question, user response, result
    SideSessionSuccess Text Text Text
  | -- | Ambiguous: question, user response
    SideSessionAmbiguous Text Text
  deriving stock (Show, Eq, Generic)

instance Disp SideSessionResult where
  disp = \case
    SideSessionSuccess q resp result ->
      section 4 (successText "✓ Side Session Complete") $
        vsep
          [ label "Question:" <+> wrappedText q,
            label "User said:" <+> "\"" <> wrappedText resp <> "\"",
            label "Result:" <+> successText (wrappedText result)
          ]
    SideSessionAmbiguous q resp ->
      section 4 (dimText "⏸ Side Session - Ambiguous") $
        vsep
          [ label "Question:" <+> wrappedText q,
            label "User said:" <+> "\"" <> wrappedText resp <> "\"",
            dimText "Could not determine answer from response"
          ]

--------------------------------------------------------------------------------
-- Session Info
--------------------------------------------------------------------------------

-- | Session information displayed at startup.
-- Shows seeded context variables from the session.
data SessionInfo = SessionInfo
  { seededContext :: [(Text, Text)] -- (slot name, value)
  }
  deriving stock (Show, Eq, Generic)

instance Disp SessionInfo where
  disp (SessionInfo ctx) =
    if null ctx
      then mempty
      else
        section 0 (label "Session initialized:") $
          vsep
            [ indent 2 $ label "•" <+> pretty slot <+> "=" <+> successText (pretty val)
            | (slot, val) <- ctx
            ]
