-- | UK Passport eligibility tool definitions.
--
-- This toolkit demonstrates a purely askable-driven proof search:
-- no mock database, no tool bindings, all facts come from user questions.
--
-- The CheckEligibility tool runs the solver directly, returning proofs found
-- so far and registering ALL blocked askables with Sentinel for dynamic Ask_ tools.
module Examples.Passport.Tools
  ( passportToolkit,
    passportSystemPrompt,
  )
where

import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Examples.Passport.Rules (brit)
import Examples.Passport.Types (PassportDB)
import Pre
import Sentinel.Context (AskableSpec (..), ContextDecl (..), ContextDecls, declareContext, emptyContextDecls)
import Sentinel.Schema qualified as Schema
import Sentinel.Sentinel (SentinelEnv (..))
import Sentinel.Solver (runSolverFull)
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry, EvidenceType (..), declareAskable, emptyAskableRegistry)
import Sentinel.Solver.Combinators (SolverEnv (..), contextVar, emptySolverState, withRule)
import Sentinel.Solver.ToolBindings (ToolBindingRegistry, emptyToolBindingRegistry)
import Sentinel.Solver.Types (ScalarType (..), SolverSuccess (..))
import Sentinel.Tool (Tool (..), ToolCategory (..), ToolGuard (..), ToolOutput (..))
import Sentinel.Toolkit (Toolkit (..))

--------------------------------------------------------------------------------
-- Toolkit
--------------------------------------------------------------------------------

passportToolkit :: Toolkit PassportDB
passportToolkit =
  Toolkit
    { tools = [checkEligibilityTool],
      systemPrompt = passportSystemPrompt,
      toolBindings = passportToolBindings,
      askables = passportAskables,
      contextDecls = passportContextDecls
    }

--------------------------------------------------------------------------------
-- System Prompt
--------------------------------------------------------------------------------

passportSystemPrompt :: Text
passportSystemPrompt =
  T.unlines
    [ "You are a helpful assistant that determines eligibility for a British passport.",
      "You help applicants understand whether they qualify for British citizenship based",
      "on the UK nationality rules. The user should specify the name of the applicant.",
      "",
      "HOW TO DETERMINE ELIGIBILITY:",
      "You MUST use the CheckEligibility tool to determine eligibility.",
      "Do NOT try to determine eligibility yourself by asking questions directly.",
      "The system has a built-in rule engine that knows the UK nationality rules.",
      "",
      "WORKFLOW:",
      "1. When a user asks about passport eligibility, IMMEDIATELY call CheckEligibility.",
      "2. The tool will respond with:",
      "   - Any proofs of eligibility found so far (with required documents)",
      "   - A count of blocked questions that need answering to explore more paths",
      "3. If PROOFS WERE FOUND, STOP and present them to the user:",
      "   - Explain the eligibility path(s) clearly.",
      "   - If there are also blocked questions, tell the user there are additional",
      "     paths that could be explored, and ASK whether they want to continue.",
      "   - Only proceed with Ask_ questions if the user wants to explore more.",
      "4. If NO proofs were found yet and there are blocked questions, use the",
      "   Ask_ tools to ask those questions to the user.",
      "5. After the user answers, call CheckEligibility again to discover more proofs.",
      "6. Repeat until all paths are explored or the user is satisfied.",
      "",
      "CRITICAL RULES:",
      "- ALWAYS call CheckEligibility as your first action — never skip this step.",
      "- After ANY Ask_ question is answered, you MUST call CheckEligibility again.",
      "  This is NON-NEGOTIABLE. The rule engine must re-run with the new facts to find proofs.",
      "  You CANNOT determine eligibility yourself — only CheckEligibility produces valid proofs.",
      "  If you skip this step, the user will receive INCORRECT information.",
      "- Your workflow MUST always be: CheckEligibility → Ask_ questions → CheckEligibility → ...",
      "  NEVER end with Ask_ questions — always follow up with CheckEligibility.",
      "- Only ask questions that the system makes available via Ask_ tools.",
      "- WHEN PROOFS ARE FOUND: Always present them immediately. Do NOT silently continue",
      "  exploring — the user deserves to know they have a valid path. If there are still",
      "  blocked questions, mention that alternative paths exist and let the user decide.",
      "- When referring to family members, use clear descriptions (e.g., 'your mother', 'your father's mother').",
      "- Be patient — citizenship can depend on parents' and grandparents' status.",
      "- Multiple proofs may exist — present all of them to the user.",
      "- NEVER give a final answer about eligibility without having called CheckEligibility",
      "  after the most recent round of Ask_ questions. If you do, you WILL give wrong information."
    ]

--------------------------------------------------------------------------------
-- Tool Bindings (none — all facts from askables)
--------------------------------------------------------------------------------

passportToolBindings :: ToolBindingRegistry
passportToolBindings = emptyToolBindingRegistry

--------------------------------------------------------------------------------
-- Context Declarations
--------------------------------------------------------------------------------

passportContextDecls :: ContextDecls
passportContextDecls =
  declareContext
    ContextDecl
      { name = "applicant",
        valueType = TextType,
        seedValue = Nothing,
        askable =
          Just
            AskableSpec
              { questionTemplate = "Who is the applicant?",
                candidates = []
              },
        description = "The person whose British citizenship eligibility is being checked"
      }
    emptyContextDecls

--------------------------------------------------------------------------------
-- CheckEligibility Tool
--------------------------------------------------------------------------------

checkEligibilityTool :: Tool PassportDB
checkEligibilityTool =
  Tool
    { name = "CheckEligibility",
      description =
        "Check British citizenship eligibility. Returns any proofs found so far "
          <> "and reports how many questions are still blocking further exploration. "
          <> "Call this repeatedly after answering blocked questions to discover more proofs.",
      params = Schema.emptyObjectSchema,
      category = DataTool,
      guard = NoGuard,
      execute = \_args -> do
        sentinelEnv <- lift ask

        -- Get current fact stores
        baseFactStore <- liftIO $ readIORef sentinelEnv.facts
        askableFactStore <- liftIO $ readIORef sentinelEnv.askableStore
        ctxStore <- liftIO $ readIORef sentinelEnv.contextStore

        -- Construct solver environment (no tool bindings, no context decls needed)
        let solverEnv =
              SolverEnv
                { toolBindings = passportToolBindings,
                  askables = passportAskables,
                  contextDecls = passportContextDecls,
                  contextStore = ctxStore,
                  invokeDataTool = \_ _ -> pure (Left "No data tools in passport example")
                }

        let initState = emptySolverState baseFactStore askableFactStore

        -- Run solver to find all proofs and collect all blocks
        let solverAction = withRule "british_citizenship" do
              person <- contextVar "applicant"
              proof <- brit person
              pure
                SolverSuccess
                  { bindings = M.empty,
                    proof = proof,
                    reason = "british_citizenship"
                  }

        -- Clear old pending inputs before the solver run
        liftIO $ writeIORef sentinelEnv.pendingUserInputs []

        (outcome, _finalState) <- liftIO $ runSolverFull "british_citizenship" solverEnv initState solverAction

        -- Return outcome — framework handles block registration, console display, and LLM text
        pure
          ToolOutput
            { observation = "",
              producedFacts = [],
              triggerSideSession = Nothing,
              solverOutcome = Just outcome
            }
    }

--------------------------------------------------------------------------------
-- Askable Declarations
--------------------------------------------------------------------------------

passportAskables :: AskableRegistry
passportAskables =
  foldl'
    (flip declareAskable)
    emptyAskableRegistry
    [ AskableDecl
        { predicate = "possibly_british",
          argumentTypes = [ExprType],
          questionTemplate = "Could {0} potentially be British?",
          evidenceType = UserStatement,
          description = "Whether a person might be British"
        },
      AskableDecl
        { predicate = "born_in_uk",
          argumentTypes = [ExprType],
          questionTemplate = "Was {0} born in the UK?",
          evidenceType = UserStatement,
          description = "Whether a person was born in the UK"
        },
      AskableDecl
        { predicate = "born_before_1983",
          argumentTypes = [ExprType],
          questionTemplate = "Was {0} born before 1983?",
          evidenceType = UserStatement,
          description = "Whether a person was born before 1983"
        },
      AskableDecl
        { predicate = "born_after_2006",
          argumentTypes = [ExprType],
          questionTemplate = "Was {0} born after 2006?",
          evidenceType = UserStatement,
          description = "Whether a person was born after 2006"
        },
      AskableDecl
        { predicate = "settled",
          argumentTypes = [ExprType],
          questionTemplate = "Did {0} have settled status in the UK at the time of their child's birth?",
          evidenceType = UserStatement,
          description = "Whether a person had settled status"
        },
      AskableDecl
        { predicate = "naturalised",
          argumentTypes = [ExprType],
          questionTemplate = "Was {0} naturalised as a British citizen?",
          evidenceType = UserStatement,
          description = "Whether a person was naturalised as British"
        },
      AskableDecl
        { predicate = "years_3_living_in_uk",
          argumentTypes = [ExprType],
          questionTemplate = "Has {0} lived in the UK for 3 or more years before their child's birth?",
          evidenceType = UserStatement,
          description = "Whether a person lived in the UK for 3+ years"
        },
      AskableDecl
        { predicate = "possibly_brit_otbd",
          argumentTypes = [ExprType],
          questionTemplate = "Is {0} possibly British otherwise than by descent (i.e. not solely through a parent born abroad)?",
          evidenceType = UserStatement,
          description = "Whether a person might be British otherwise than by descent"
        },
      AskableDecl
        { predicate = "crown_service",
          argumentTypes = [ExprType],
          questionTemplate = "Was {0} in Crown service at the time of their child's birth?",
          evidenceType = UserStatement,
          description = "Whether a person was in Crown service"
        },
      AskableDecl
        { predicate = "married",
          argumentTypes = [ExprType, ExprType],
          questionTemplate = "Were {0} and {1} married at the time of their child's birth?",
          evidenceType = UserStatement,
          description = "Whether two people were married at time of birth"
        }
    ]
