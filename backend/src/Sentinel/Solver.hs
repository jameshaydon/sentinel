-- | Sentinel Solver - LogicT-based proof search with tool invocation.
--
-- The solver evaluates queries against a rulebase, producing proof traces.
-- It can:
--
-- - __Query predicates__: Check fact store, invoke tools to establish facts
-- - __Apply rules__: Combine predicates with logical operators (allOf, oneOf)
-- - __Block on user input__: Report when context or user confirmation is needed
-- - __Produce proofs__: Full audit trail of how conclusions were reached
--
-- == Architecture
--
-- The solver sits between the LLM Agent and the fact stores:
--
-- @
--                         ┌─────────────────┐
--                         │    LLM Agent    │
--                         └────────┬────────┘
--                                  │
--                         ┌────────▼────────┐
--                         │     Solver      │
--                         │  (this module)  │
--                         └────────┬────────┘
--                                  │
--          ┌───────────────────────┼───────────────────────┐
--          │                       │                       │
--   ┌──────▼──────┐         ┌──────▼──────┐         ┌──────▼──────┐
--   │ Fact Store  │         │   Context   │         │  Data Tools │
--   │             │         │    Store    │         │ (via Sentinel)│
--   └─────────────┘         └─────────────┘         └─────────────┘
-- @
--
-- == Example Usage
--
-- @
-- -- Define a rule
-- eligibleForRefund :: SolverM SolverSuccess
-- eligibleForRefund = do
--   booking <- contextVar "booking_of_interest"
--   withRule "airline_fault" $ withReason "airline_fault" $ do
--     flight <- queryPredicate "booking_flight" [booking]
--     status <- queryPredicate "flight_status" [flight.arguments !! 1]
--     require (status.arguments !! 1 == ScStr "cancelled") "flight_cancelled"
--     bindings <- gets (.bindings)
--     proof <- getCurrentProof
--     pure $ SolverSuccess
--       { bindings = M.singleton "RefundType" (ScStr "full")
--       , proof = fromMaybe (RuleApplied "airline_fault" []) proof
--       , reason = "airline_fault"
--       }
--
-- -- Run the solver
-- result <- runSolver env state eligibleForRefund
-- case result of
--   Success successes -> -- one or more proofs found
--   BlockedOnUserInput block -> -- need user input (context or confirmation)
--   Failure failures -> -- all paths failed
-- @
module Sentinel.Solver
  ( -- * Re-exports from Types
    module Sentinel.Solver.Types,

    -- * Re-exports from Combinators
    module Sentinel.Solver.Combinators,

    -- * Re-exports from ToolBindings
    module Sentinel.Solver.ToolBindings,

    -- * Re-exports from Askable
    module Sentinel.Solver.Askable,

    -- * Re-exports from Facts
    BaseFactStore,
    AskableFactStore,
    emptyBaseFactStore,
    emptyAskableFactStore,

    -- * Running the Solver
    runSolverFull,
  )
where

import Pre
import Sentinel.Facts (AskableFactStore, BaseFactStore, emptyAskableFactStore, emptyBaseFactStore)
import Sentinel.Solver.Askable
import Sentinel.Solver.Combinators
import Sentinel.Solver.ToolBindings
import Sentinel.Solver.Types

-- | Run a solver query and return a full 'SolverOutcome'.
--
-- Unlike 'runSolver' which collapses to a single 'SolverResult' case,
-- this captures all successes, all blocks, and all failures at once.
-- This is the preferred entry point when you need the complete picture
-- (e.g. for formatting solver results for the LLM).
runSolverFull ::
  Text ->
  SolverEnv ->
  SolverState ->
  SolverM SolverSuccess ->
  IO (SolverOutcome, SolverState)
runSolverFull goalName env initState solver = do
  (successes, finalState) <- runSolverM env initState solver
  let blocks = dedupBlocks finalState.pendingUserInputs
      failures = getFailedPaths finalState
  pure
    ( SolverOutcome
        { goalName = goalName,
          successes = successes,
          blocked = blocks,
          failures = failures
        },
      finalState
    )
