-- | Sentinel Solver - LogicT-based proof search with tool invocation.
--
-- The solver evaluates queries against a rulebase, producing proof traces.
-- It can:
--
-- - __Query predicates__: Check fact store, invoke tools to establish facts
-- - __Apply rules__: Combine predicates with logical operators (allOf, oneOf)
-- - __Block on context__: Report when a context variable needs to be established
-- - __Block on askables__: Report when user confirmation is needed
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
--   BlockedOnContext block -> -- need user to establish context
--   BlockedOnAskable block -> -- need user confirmation
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
    runSolver,
    runSolverWith,
  )
where

import Data.List.NonEmpty qualified as NE
import Pre
import Sentinel.Facts (AskableFactStore, BaseFactStore, emptyAskableFactStore, emptyBaseFactStore)
import Sentinel.Solver.Askable
import Sentinel.Solver.Combinators
import Sentinel.Solver.ToolBindings
import Sentinel.Solver.Types

-- | Run a solver query and return a SolverResult.
--
-- This is the main entry point for the solver. It:
-- 1. Runs the solver monad collecting all successful proofs
-- 2. If no successes, checks for pending blocks (askables or context)
-- 3. Returns Success, BlockedOnContext, BlockedOnAskable, or Failure
runSolver ::
  SolverEnv ->
  SolverState ->
  SolverM SolverSuccess ->
  IO (SolverResult, SolverState)
runSolver env initState solver = do
  (successes, finalState) <- runSolverM env initState solver
  case NE.nonEmpty successes of
    Just ne ->
      -- At least one proof succeeded
      pure (Success ne, finalState)
    Nothing ->
      -- No successes - check for pending blocks
      -- Priority: context blocks first (need context to make progress),
      -- then askables (need user confirmation)
      case finalState.pendingContextBlocks of
        (block : _) ->
          pure (BlockedOnContext block, finalState)
        [] -> case finalState.pendingAskables of
          (block : _) ->
            pure (BlockedOnAskable block, finalState)
          [] ->
            -- No pending blocks - all paths truly failed
            -- Use the recorded failure paths for detailed diagnostics
            let failures = case getFailedPaths finalState of
                  [] ->
                    -- Shouldn't happen, but fallback to generic message
                    [ FailurePath
                        { ruleName = fromMaybe "unknown" finalState.currentRule,
                          reason = "No proof path succeeded",
                          partialProof = Nothing
                        }
                    ]
                  fps -> fps
             in pure (Failure failures, finalState)

-- | Run a solver query with explicit initial facts.
--
-- Convenience wrapper that constructs the SolverState from fact stores.
runSolverWith ::
  SolverEnv ->
  BaseFactStore ->
  AskableFactStore ->
  SolverM SolverSuccess ->
  IO (SolverResult, SolverState)
runSolverWith env baseFacts askableFacts solver =
  runSolver env (emptySolverState baseFacts askableFacts) solver
