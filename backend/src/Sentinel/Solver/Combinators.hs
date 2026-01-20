-- | Solver combinators for the Sentinel solver.
--
-- The solver uses LogicT for backtracking proof search. It can:
-- - Query predicates (checking fact store, invoking tools if needed)
-- - Apply rules with sub-proofs
-- - Block on missing context or user confirmation
--
-- The design uses state-based blocking: when the solver encounters a blocking
-- condition (missing context or unconfirmed askable), it records the block in
-- state and fails that branch with 'mzero'. LogicT then backtracks to try
-- other paths. After all paths are exhausted, if there are pending blocks
-- recorded, the solver reports "blocked on X" rather than outright failure.
module Sentinel.Solver.Combinators
  ( -- * Solver Monad
    SolverM,
    SolverEnv (..),
    SolverState (..),

    -- * Running the Solver
    runSolverM,
    emptySolverState,

    -- * Core Combinators
    queryPredicate,
    andAll,
    oneOf,
    require,
    withRule,
    withReason,

    -- * Context and Askables
    contextVar,
    askable,

    -- * Fact Store Operations
    addFact,
    addFacts,
    getFacts,

    -- * Proof Construction
    getCurrentProof,
    appendProof,

    -- * Failure
    failWith,

    -- * State Accessors
    getPendingAskables,
    getPendingContextBlocks,
    getFailedPaths,
    getCurrentRule,
  )
where

import Control.Monad.Logic (LogicT, observeAllT)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Data.Aeson (Value)
import Pre
import Sentinel.Context (ContextDecl (..), ContextDecls, ContextStore, getContext, lookupContextDecl)
import Sentinel.Facts
  ( AskableFactStore,
    BaseFactStore,
    addBaseFact,
    addBaseFacts,
    lookupAskableFact,
    lookupBaseFacts,
  )
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry, formatQuestion, lookupAskable)
import Sentinel.Solver.ToolBindings (ToolBinding (..), ToolBindingRegistry, lookupBinding)
import Sentinel.Solver.Types

--------------------------------------------------------------------------------
-- Solver Environment
--------------------------------------------------------------------------------

-- | Read-only environment for the solver.
data SolverEnv = SolverEnv
  { -- | Registry of tool bindings (predicate -> tool)
    toolBindings :: ToolBindingRegistry,
    -- | Registry of askable predicate declarations
    askables :: AskableRegistry,
    -- | Context variable declarations
    contextDecls :: ContextDecls,
    -- | Current context values
    contextStore :: ContextStore,
    -- | Callback to invoke a data tool.
    -- Takes tool name and JSON args, returns produced facts or error.
    invokeDataTool :: Text -> Value -> IO (Either Text [BaseFact])
  }

--------------------------------------------------------------------------------
-- Solver State
--------------------------------------------------------------------------------

-- | Mutable state for the solver.
data SolverState = SolverState
  { -- | Base facts established during proof
    baseFactStore :: BaseFactStore,
    -- | Askable facts (user confirmations)
    askableFactStore :: AskableFactStore,
    -- | Current proof being constructed (stack of sub-proofs)
    proofStack :: [Proof],
    -- | Current rule name (for error messages)
    currentRule :: Maybe Text,
    -- | Current reason (for success reporting)
    currentReason :: Maybe Text,
    -- | Askable predicates that blocked proof paths (recorded for reporting)
    pendingAskables :: [AskableBlock],
    -- | Context variables that blocked proof paths (recorded for reporting)
    pendingContextBlocks :: [ContextBlock],
    -- | Failed proof paths (recorded for diagnostic reporting)
    failedPaths :: [FailurePath]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty solver state.
emptySolverState :: BaseFactStore -> AskableFactStore -> SolverState
emptySolverState baseFacts askableFacts =
  SolverState
    { baseFactStore = baseFacts,
      askableFactStore = askableFacts,
      proofStack = [],
      currentRule = Nothing,
      currentReason = Nothing,
      pendingAskables = [],
      pendingContextBlocks = [],
      failedPaths = []
    }

--------------------------------------------------------------------------------
-- Solver Monad
--------------------------------------------------------------------------------

-- | The solver monad.
--
-- Layers:
-- - LogicT: Backtracking for alternative proof paths
-- - StateT: Mutable fact store and proof construction
-- - ReaderT: Immutable environment (registries, tool callback)
-- - IO: For tool invocation
type SolverM = LogicT (StateT SolverState (ReaderT SolverEnv IO))

-- | Run the solver and collect all successful proof paths.
--
-- Returns a list of successful proofs (may be empty if all paths failed)
-- along with the final state (which includes any pending blocks recorded).
runSolverM ::
  SolverEnv ->
  SolverState ->
  SolverM SolverSuccess ->
  IO ([SolverSuccess], SolverState)
runSolverM env initState solver =
  runReaderT (runStateT (observeAllT solver) initState) env

--------------------------------------------------------------------------------
-- Core Combinators
--------------------------------------------------------------------------------

-- | Query a predicate, invoking a tool if the fact is not in the store.
--
-- This is the primary way to establish base facts. The solver:
-- 1. Checks if a matching fact exists in the fact store
-- 2. If not, looks up the tool binding for the predicate
-- 3. Invokes the tool to establish facts
-- 4. Returns matching facts (using LogicT for multiple matches)
--
-- The first N arguments (where N = inputArity from the binding) must be
-- ground (known values). Remaining arguments are unified with the tool output.
queryPredicate :: Text -> [Scalar] -> SolverM BaseFact
queryPredicate predName inputArgs = do
  -- First check the fact store
  existingFacts <- gets (lookupBaseFacts predName . (.baseFactStore))
  let matchingFacts = filter (matchesFact inputArgs) existingFacts

  case matchingFacts of
    (f : fs) -> do
      -- Have matching facts, return one (backtracking can try others)
      appendProof (FactUsed f)
      asum $ pure <$> (f : fs)
    [] -> do
      -- Need to invoke tool
      registry <- asks (.toolBindings)
      case lookupBinding predName registry of
        Nothing ->
          -- No tool binding - this predicate cannot be established
          failWith $ "No tool binding for predicate: " <> predName
        Just ToolBinding {inputArity, toolName, buildArgs} -> do
          -- Validate we have enough input args
          when (length inputArgs < inputArity) $
            failWith $
              "Predicate "
                <> predName
                <> " requires "
                <> tshow inputArity
                <> " input args, got "
                <> tshow (length inputArgs)

          -- Invoke the tool
          let toolInput = buildArgs (take inputArity inputArgs)
          invokeTool <- asks (.invokeDataTool)
          toolResult <- liftIO $ invokeTool toolName toolInput

          case toolResult of
            Left err ->
              failWith $ "Tool " <> toolName <> " failed: " <> err
            Right newFacts -> do
              -- Store all facts produced by the tool
              addFacts newFacts

              -- Find matching facts
              let matches = filter (matchesFact inputArgs) newFacts
              case matches of
                [] ->
                  failWith $
                    "Tool "
                      <> toolName
                      <> " did not establish "
                      <> predName
                (f : fs) -> do
                  appendProof (FactUsed f)
                  asum $ pure <$> (f : fs)
  where
    -- Check if a fact matches the query arguments
    matchesFact :: [Scalar] -> BaseFact -> Bool
    matchesFact args fact =
      fact.predicateName == predName
        && matchesArgs args fact.arguments

    -- Match query args against fact args (query may have fewer args)
    matchesArgs :: [Scalar] -> [Scalar] -> Bool
    matchesArgs queryArgs factArgs =
      length queryArgs <= length factArgs
        && and (zipWith (==) queryArgs factArgs)

-- | All premises must succeed. Collects proofs from each.
andAll :: [SolverM Proof] -> SolverM Proof
andAll premises = do
  proofs <- traverse id premises
  rule <- gets (.currentRule)
  pure $ RuleApplied (fromMaybe "all_of" rule) proofs

-- | Try alternatives in order, backtracking on failure.
--
-- Uses LogicT's MonadPlus to provide backtracking.
oneOf :: [SolverM a] -> SolverM a
oneOf = asum

-- | Require a boolean condition to hold.
require :: Bool -> Text -> SolverM Proof
require condition desc
  | condition = pure $ RuleApplied ("require: " <> desc) []
  | otherwise = failWith $ "Requirement failed: " <> desc

-- | Run a sub-proof under a named rule.
--
-- The rule name is recorded in the proof trace.
withRule :: Text -> SolverM a -> SolverM a
withRule ruleName action = do
  oldRule <- gets (.currentRule)
  modify' $ \s -> s {currentRule = Just ruleName}
  result <- action
  modify' $ \s -> s {currentRule = oldRule}
  pure result

-- | Set the reason for a successful proof.
--
-- The reason is included in SolverSuccess for human-readable output.
withReason :: Text -> SolverM a -> SolverM a
withReason reason action = do
  modify' $ \s -> s {currentReason = Just reason}
  action

--------------------------------------------------------------------------------
-- Context Variables
--------------------------------------------------------------------------------

-- | Get a context variable value, or block if not established.
--
-- If the context variable is not established, this records a ContextBlock
-- in state and fails the branch with 'mzero'. LogicT will backtrack to try
-- other paths. The pending block can be checked after all paths are exhausted.
contextVar :: Text -> SolverM Scalar
contextVar varName = do
  ctxStore <- asks (.contextStore)
  case getContext varName ctxStore of
    Just value -> do
      appendProof (ContextBound varName value)
      pure value
    Nothing -> do
      -- Not established - look up declaration to find candidates
      ctxDecls <- asks (.contextDecls)
      case lookupContextDecl varName ctxDecls of
        Nothing ->
          -- Unknown context variable - this is an error, fail the branch
          failWith $ "Unknown context variable: " <> varName
        Just decl -> do
          -- Try to find candidates via candidateQuery
          candidates <- getCandidates decl
          -- Record the block and fail this branch
          partialProof <- getCurrentProof
          let block =
                ContextBlock
                  { slot = varName,
                    candidates = candidates,
                    partialProof = partialProof
                  }
          modify' $ \s -> s {pendingContextBlocks = block : s.pendingContextBlocks}
          mzero -- Fail this branch, allowing backtracking

-- | Get candidate values for a context variable.
getCandidates :: ContextDecl -> SolverM [ContextCandidate]
getCandidates decl = do
  case decl.candidateQuery of
    Nothing -> pure [] -- No query, no candidates
    Just queryPred -> do
      -- Query the predicate to find candidates
      -- This is a "soft" query - we don't fail if no results
      facts <- gets (lookupBaseFacts queryPred . (.baseFactStore))
      pure
        [ ContextCandidate
            { value = arg,
              description = dispScalar arg
            }
          | fact <- facts,
            arg <- take 1 (drop 1 fact.arguments) -- Second argument is typically the value
        ]
  where
    dispScalar = \case
      ScBool b -> if b then "yes" else "no"
      ScNum n -> tshow n
      ScStr t -> t

--------------------------------------------------------------------------------
-- Askable Predicates
--------------------------------------------------------------------------------

-- | Check an askable predicate, or block if not yet confirmed.
--
-- If the predicate has been confirmed (True), this succeeds.
-- If the predicate has been denied (False), this fails.
-- If the predicate hasn't been asked yet, this records an AskableBlock
-- in state and fails the branch with 'mzero'. LogicT will backtrack to try
-- other paths. The pending block can be checked after all paths are exhausted.
askable :: Text -> [Scalar] -> SolverM Proof
askable predName args = do
  -- Check if already answered
  askableStore <- gets (.askableFactStore)
  case lookupAskableFact predName args askableStore of
    Just True -> do
      -- User confirmed
      let fact = BaseFact predName args
      pure $ FactUsed fact
    Just False -> do
      -- User denied
      failWith $ "User denied: " <> predName
    Nothing -> do
      -- Not yet asked - look up declaration
      registry <- asks (.askables)
      case lookupAskable predName registry of
        Nothing ->
          failWith $ "Unknown askable predicate: " <> predName
        Just decl -> do
          let question = formatQuestion decl.questionTemplate args
          partialProof <- getCurrentProof
          let block =
                AskableBlock
                  { predicate = predName,
                    question = question,
                    arguments = args,
                    partialProof = partialProof
                  }
          modify' $ \s -> s {pendingAskables = block : s.pendingAskables}
          mzero -- Fail this branch, allowing backtracking

--------------------------------------------------------------------------------
-- Fact Store Operations
--------------------------------------------------------------------------------

-- | Add a single fact to the store.
addFact :: BaseFact -> SolverM ()
addFact fact = modify' $ \s ->
  s {baseFactStore = addBaseFact fact s.baseFactStore}

-- | Add multiple facts to the store.
addFacts :: [BaseFact] -> SolverM ()
addFacts facts = modify' $ \s ->
  s {baseFactStore = addBaseFacts facts s.baseFactStore}

-- | Get all facts for a predicate.
getFacts :: Text -> SolverM [BaseFact]
getFacts predName = gets (lookupBaseFacts predName . (.baseFactStore))

--------------------------------------------------------------------------------
-- Proof Construction
--------------------------------------------------------------------------------

-- | Get the current proof (combines proof stack).
getCurrentProof :: SolverM (Maybe Proof)
getCurrentProof = do
  stack <- gets (.proofStack)
  case stack of
    [] -> pure Nothing
    [p] -> pure (Just p)
    ps -> do
      rule <- gets (.currentRule)
      pure $ Just $ RuleApplied (fromMaybe "proof" rule) ps

-- | Append a proof step to the current proof stack.
appendProof :: Proof -> SolverM ()
appendProof proof = modify' $ \s ->
  s {proofStack = s.proofStack ++ [proof]}

--------------------------------------------------------------------------------
-- Failure
--------------------------------------------------------------------------------

-- | Fail with an error message.
--
-- Records the failure reason in state for later diagnostic reporting,
-- then uses LogicT's mzero to backtrack and try alternatives.
failWith :: Text -> SolverM a
failWith msg = do
  ruleName <- gets (fromMaybe "unknown" . (.currentRule))
  partialProof <- getCurrentProof
  let failure =
        FailurePath
          { ruleName = ruleName,
            reason = msg,
            partialProof = partialProof
          }
  modify' $ \s -> s {failedPaths = failure : s.failedPaths}
  mzero

--------------------------------------------------------------------------------
-- State Accessors
--------------------------------------------------------------------------------

-- | Get pending askable blocks from solver state.
getPendingAskables :: SolverState -> [AskableBlock]
getPendingAskables s = s.pendingAskables

-- | Get pending context blocks from solver state.
getPendingContextBlocks :: SolverState -> [ContextBlock]
getPendingContextBlocks s = s.pendingContextBlocks

-- | Get failed proof paths from solver state.
getFailedPaths :: SolverState -> [FailurePath]
getFailedPaths s = s.failedPaths

-- | Get current rule from solver state.
getCurrentRule :: SolverState -> Maybe Text
getCurrentRule s = s.currentRule

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

tshow :: (Show a) => a -> Text
tshow = fromString . show
