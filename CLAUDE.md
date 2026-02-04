# Sentinel

Sentinel is a **governance middleware framework** for LLM agents that adds reasoning using logic-programming techniques. It sits between an LLM agent and its tools, controlling what facts can be established and what actions can be taken through declarative guards and solver-based proof search.

After a task, update CLAUDE.md with any changes/insights.

## Commands
```bash
just build  # Build the project (runs hpack automatically, then cabal build -O0). Always use this.
just weeder # Detect dead code (use when task complete)
just test   # Test (no tests for now, ignore)
nix build   # build the nix package
nix flake check # run all nix flake checks
```

## Running
```bash
cabal run repl -- --example airlogic --user usr_sarah_chen
cabal run repl -- --example aircanada --user usr_james_doe
cabal run repl -- --example passport --user usr_test
cabal run repl -- --help  # Show CLI options
```

## Navigation Guide

### Core Logic (start here for semantic changes)
- `Solver/` — Proof search, backtracking, the heart of the system
- `Sentinel.hs` — Core types & middleware
- `Tool.hs` — Tool, Guard, ToolGuard types
- `Toolkit.hs` — Toolkit builder & guard evaluation
- `Facts.hs` — Fact store infrastructure
- `Context.hs` — Context variable management
- `Agent.hs` — ReAct agent loop

### Infrastructure (rarely need to touch)
- `Output.hs`, `Pretty.hs` — Console formatting, colors, display
- `JSON.hs`, `Schema.hs` — JSON serialization utilities
- `LLM.hs` — OpenAI API integration
- `App.hs` — CLI argument parsing & example loading
- `Example.hs` — Example packaging & REPL scaffolding

### Common Tasks
| Task | Look at | Skip |
|------|---------|------|
| Change proof search behavior | `Solver/Combinators.hs`, `Solver/Types.hs` | Output, Pretty |
| Add/modify guards | `Toolkit.hs`, example `Tools.hs` | Output, JSON, Schema |
| Change fact storage | `Facts.hs` | Output, Pretty, LLM |
| Modify agent behavior | `Agent.hs` | Output, App |
| Add domain example | `Examples/` directory | Core Sentinel modules |

## Architecture Overview

- User: converses with LLM agent
- Agent (ReAct loop): calls LLM (with tool defs), calls tools guarded by Sentinel, queries sentinel
- Sentinel: guards tool calls, evaluates guards with solver, manages fact store
- Solver: proves goals, backtracking implementation, auto-invoke data tools, produces proof trees or lists what is blocking

### Key Concepts

- **DataTool**: Auto-invoked by the solver to establish facts (e.g., `RetrieveBooking`, `CheckFlightStatus`)
- **ActionTool**: Requires explicit LLM invocation, may have side effects (e.g., `InitiateRefund`)
- **Guard**: A `Value -> SolverM Proof` function that must succeed before an ActionTool can execute
- **Context Variable**: A named slot tracking conversational focus (e.g., `booking_of_interest`)
- **Askable Predicate**: A predicate establishable by asking the user (e.g., `user_claims_bereavement`)
- **Fact Store**: Working memory of ground predicates established during the session
- **Proof Trace**: Audit trail showing how conclusions were derived

### SolverOutcome

`SolverOutcome` captures the full picture from a solver run:
- **successes**: All proofs found (may be multiple)
- **blocked**: All user input blocks (deduplicated)
- **failures**: All failed proof paths

`runSolverFull` (in `Solver.hs`) returns `SolverOutcome`. The framework automatically:
- Registers blocked items with Sentinel for dynamic Ask_ tool generation
- Displays solver outcomes on the console via `Disp SolverOutcome`
- Formats solver outcomes as text for the LLM via `formatSolverOutcomeForLLM`

`ToolOutput.solverOutcome :: Maybe SolverOutcome` lets tools that run the solver internally (like `CheckEligibility`) return their outcome for framework processing.

`SentinelResult.AskUser` carries a `Text` field with the formatted solver outcome for the LLM.

## Project Structure
```
backend/
├── app/Main.hs                          # Entry point
├── src/
│   ├── Pre.hs                           # Custom prelude
│   ├── Sentinel/
│   │   │
│   │   │  # ══ Core Logic ══
│   │   ├── Sentinel.hs                  # Core types & middleware
│   │   ├── Tool.hs                      # Tool, Guard, ToolGuard types
│   │   ├── Toolkit.hs                   # Toolkit builder & guard evaluation
│   │   ├── Facts.hs                     # Fact store
│   │   ├── Context.hs                   # Context variable management
│   │   ├── Agent.hs                     # ReAct agent loop
│   │   ├── Solver.hs                    # Solver entry point (re-exports)
│   │   └── Solver/
│   │       ├── Types.hs                 # Scalar, BaseFact, Proof, SolverResult, SolverOutcome
│   │       ├── Combinators.hs           # SolverM monad (LogicT-based)
│   │       ├── ToolBindings.hs          # Predicate-to-tool mappings
│   │       └── Askable.hs               # User-confirmable predicates
│   │   │
│   │   │  # ══ Infrastructure (rarely relevant) ══
│   │   ├── Output.hs                    # Console formatting only
│   │   ├── Pretty.hs                    # Pretty-printing only
│   │   ├── JSON.hs                      # JSON utilities
│   │   ├── Schema.hs                    # JSON schema helpers
│   │   ├── LLM.hs                       # OpenAI integration
│   │   ├── App.hs                       # CLI & example loading
│   │   └── Example.hs                   # Example packaging
│   │
│   └── Examples/
│       ├── AirCanada/
│       │   ├── Example.hs               # Configuration & entry point
│       │   ├── MockDB.hs                # Sample database
│       │   ├── Refund.hs                # Refund calculation logic
│       │   ├── ToolBindings.hs          # Predicate-to-tool mappings
│       │   ├── Tools.hs                 # Full toolkit with guards
│       │   └── Types.hs                 # Domain types
│       ├── AirLogic/
│       │   └── ...
│       └── Passport/
│           ├── Example.hs               # Configuration & entry point
│           ├── Rules.hs                 # Citizenship proof rules (SolverM)
│           ├── Tools.hs                 # Toolkit, askables, system prompt
│           └── Types.hs                 # PersonId (Scalar), PassportDB
├── test/Main.hs
└── package.yaml                         # Hpack config (source of truth)

examples/
└── air-canada/
    └── easy.md                          # Air Canada refund rules
```

## Code Style

- **Record dot syntax**: `person.name` not `name person`
- **No implicit prelude**: All modules use `Pre`
- **Qualified imports**: `import Sentinel.Tool qualified as Tool` (exception: small coupled modules in same directory)
- **No module prefixes on exports**: Export `Config` not `ToolConfig`, use as `Tool.Config`
- **GHC2024**: `BlockArguments`, `LambdaCase`, `OverloadedStrings`
- **Strict warnings**: `-Werror -Weverything`
- Use `lens` and `generic-lens`: `over #foo (+ 1)`

## Key Patterns

- **ReAct Agent**: LLM generates `Thought → Action → Observation` loops until `Final Answer`
- **Toolkit abstraction**: `Toolkit db` bundles tools, bindings, askables, context decls, system prompt
- **Stateless solver**: Complete result per invocation, no continuation between turns
- **Tool categories**: DataTools auto-invoked by solver; ActionTools need explicit LLM invocation
- **Function-based guards**: `Value -> SolverM Proof` unifies guards with rule system
- **Generic deriving**: JSON via `DeriveAnyClass` and `aeson` (except `Scalar` which has manual instances)
- **Compound terms**: `Scalar` supports `ScExpr Text [Scalar]` for structured values like `mother(applicant)`. Nullary `ScExpr "x" []` = atom, compound `ScExpr "f" [args]` = function application. `ScalarType` has a corresponding `ExprType`.

## Solver Combinators (Sentinel.Solver.Combinators)
```haskell
queryPredicate :: Text -> [Scalar] -> SolverM [Scalar]  -- Fetch/derive fact
oneOf :: [SolverM a] -> SolverM a                       -- Backtracking alternatives
andAll :: [SolverM Proof] -> SolverM [Proof]            -- All must succeed
require :: Bool -> Text -> SolverM Proof                -- Boolean condition
withRule :: Text -> SolverM a -> SolverM a              -- Name a proof step
contextVar :: Text -> SolverM Scalar                    -- Get context or block
askable :: Text -> [Scalar] -> SolverM Proof            -- User confirm or block
orElse :: SolverM a -> SolverM a -> SolverM a           -- Committed-choice fallback (ifte)
ifThenElse :: SolverM Proof -> SolverM Proof -> SolverM Proof -> SolverM Proof  -- Conditional
failWith :: Text -> SolverM a                           -- Fail with diagnostic
```

## Custom Prelude (Pre.hs)

- **`??:`** (infixr 0): Lift `Maybe` into `MonadError`. `lookupUser userId ??: UserNotFound userId`
- **`??%`** (infixr 0): Lift `Either` into `MonadError`. `parseInput raw ??% \e -> InvalidInput e`

## Passport Example — Iterative Proof Pattern

The passport example uses a different pattern from AirCanada/AirLogic:
- `CheckEligibility` is a **DataTool with NoGuard** (not an ActionTool with SolverGuardT)
- It runs `runSolverFull` in its `execute` function and returns the `SolverOutcome` in `ToolOutput.solverOutcome`
- The framework handles: block registration with Sentinel, console display via `Disp SolverOutcome`, and LLM text via `formatSolverOutcomeForLLM`
- Uses `oneOf` (not `orElse`) in `brit` and `viaParent` to explore all branches (both parents, both birth/naturalisation)
- No `withVerification` needed — the tool manages solver interaction directly

This enables iterative deepening: each call discovers proofs at the current knowledge level and reports what questions would unlock more paths.

The `applicant` is a **context variable** (not a constant). When the solver runs `contextVar "applicant"`, it either resolves to the set value (e.g., `ScStr "Romi Haydon"`) or blocks, causing the framework to create an `Ask_applicant` tool. Person identifiers use `ScExpr` compound terms (e.g., `ScExpr "mother" [ScStr "Romi Haydon"]`), which render natively as `mother(Romi Haydon)` via `scalarToText`. Helper functions `motherOf` and `fatherOf` are in `Types.hs`.

Pre-check askables use `possibly_british` / `possibly_brit_otbd` naming to clarify they are pruning checks, not standalone proofs.

## Current State

This is a prototype/demo. See `architecture.md` for future goals (MCP integration, fact TTL, hypothetical queries, loop detection).
