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
    queryAll,
    andAll,
    oneOf,
    require,
    withRule,

    -- * Context and Askables
    contextVar,
    askable,

    -- * Proof Construction
    getCurrentProof,
    appendProof,

    -- * Committed Choice
    orElse,
    ifThenElse,

    -- * Failure
    failWith,

    -- * Tool Argument Extraction
    extractArg,

    -- * State Accessors
    getFailedPaths,
  )
where

import Control.Monad.Logic (LogicT, ifte, observeAllT, once)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Data.Aeson (Value)
import Data.Text qualified as T
import Pre
import Sentinel.Context (AskableSpec (..), ContextDecl (..), ContextDecls, ContextStore, getContext, lookupContextDecl)
import Sentinel.Facts
  ( AskableFactStore,
    BaseFactStore,
    HasFactStore (..),
    addBaseFact,
    addBaseFacts,
    lookupAskableFact,
    lookupBaseFacts,
  )
import Sentinel.Solver.Askable (AskableDecl (..), AskableRegistry, formatQuestion, lookupAskable)
import Sentinel.JSON qualified as JSON
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
    -- | Current rule name and arguments (for error messages and proof construction)
    currentRule :: Maybe (Text, [Scalar]),
    -- | Current reason (for success reporting)
    currentReason :: Maybe Text,
    -- | User input requests that blocked proof paths
    pendingUserInputs :: [UserInputBlock],
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
      pendingUserInputs = [],
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

instance HasFactStore SolverM where
  addFact fact = modify' $ \s ->
    s {baseFactStore = addBaseFact fact s.baseFactStore}
  addFacts facts = modify' $ \s ->
    s {baseFactStore = addBaseFacts facts s.baseFactStore}
  getFactStore = gets (.baseFactStore)

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

-- | Query a predicate and return all matching facts as a plain list.
--
-- Unlike 'queryPredicate' which branches via LogicT (one fact per alternative),
-- 'queryAll' triggers the data tool (if facts aren't already in the store) using
-- 'once', then reads all matching facts from the store as a plain list.
-- This is useful when you need the complete set of facts (e.g., to compute
-- candidate options for 'contextVar') without introducing backtracking branches.
queryAll :: Text -> [Scalar] -> SolverM [BaseFact]
queryAll predName inputArgs = do
  -- Trigger the data tool via once (we just want the side-effect of populating the store)
  _ <- once (queryPredicate predName inputArgs) <|> pure (BaseFact predName inputArgs)
  -- Now read all matching facts from the store
  allFacts <- gets (lookupBaseFacts predName . (.baseFactStore))
  pure $ filter (matchesFact inputArgs) allFacts
  where
    matchesFact :: [Scalar] -> BaseFact -> Bool
    matchesFact args fact =
      fact.predicateName == predName
        && matchesArgs args fact.arguments

    matchesArgs :: [Scalar] -> [Scalar] -> Bool
    matchesArgs queryArgs factArgs =
      length queryArgs <= length factArgs
        && and (zipWith (==) queryArgs factArgs)

-- | All premises must succeed. Collects proofs from each.
andAll :: [SolverM Proof] -> SolverM Proof
andAll premises = do
  proofs <- traverse id premises
  pure $ RuleApplied "all_of" [] proofs

-- | Try alternatives in order, backtracking on failure.
--
-- Uses LogicT's MonadPlus to provide backtracking.
oneOf :: [SolverM a] -> SolverM a
oneOf = asum

-- | Require a boolean condition to hold.
require :: Bool -> Text -> SolverM Proof
require condition desc
  | condition = pure $ RuleApplied ("require: " <> desc) [] []
  | otherwise = failWith $ "Requirement failed: " <> desc

-- | Run a sub-proof under a named rule.
--
-- The rule name and arguments are recorded in the proof trace.
withRule :: Text -> [Scalar] -> SolverM a -> SolverM a
withRule ruleName ruleArgs action = do
  oldRule <- gets (.currentRule)
  modify' $ \s -> s {currentRule = Just (ruleName, ruleArgs)}
  result <- action
  modify' $ \s -> s {currentRule = oldRule}
  pure result

--------------------------------------------------------------------------------
-- Context Variables
--------------------------------------------------------------------------------

-- | Get a context variable value, or block if not established.
--
-- If the context variable is not established, this records a UserInputBlock
-- in state and fails the branch with 'mzero'. LogicT will backtrack to try
-- other paths. The pending block can be checked after all paths are exhausted.
--
-- The @options@ parameter controls the candidate values presented to the user:
-- - @Nothing@: free-form text input (or uses static candidates from the declaration)
-- - @Just candidates@: clickable choices computed at solver time
contextVar :: Text -> Maybe [Scalar] -> SolverM Scalar
contextVar varName options = do
  ctxStore <- asks (.contextStore)
  case getContext varName ctxStore of
    Just value -> do
      appendProof (ContextBound varName value)
      pure value
    Nothing -> do
      -- Not established - look up declaration
      ctxDecls <- asks (.contextDecls)
      case lookupContextDecl varName ctxDecls of
        Nothing ->
          -- Unknown context variable - this is an error, fail the branch
          failWith $ "Unknown context variable: " <> varName
        Just decl -> do
          -- Check if this context is askable
          case decl.askable of
            Nothing ->
              -- Not askable - can only be pre-seeded
              failWith $ "Context variable '" <> varName <> "' is not askable and not seeded"
            Just spec -> do
              -- Record the block and fail this branch
              -- Use caller-provided options if given, otherwise fall back to static candidates
              let candidates = fromMaybe spec.candidates options
              partialProof <- getCurrentProof
              let userInputBlock =
                    UserInputBlock
                      { inputType = ContextInput,
                        name = varName,
                        question = spec.questionTemplate,
                        arguments = [],
                        candidates = candidates,
                        partialProof = partialProof
                      }
              modify' $ \s ->
                s {pendingUserInputs = userInputBlock : s.pendingUserInputs}
              mzero -- Fail this branch, allowing backtracking

--------------------------------------------------------------------------------
-- Askable Predicates
--------------------------------------------------------------------------------

-- | Check an askable predicate, or block if not yet confirmed.
--
-- If the predicate has been confirmed (True), this succeeds.
-- If the predicate has been denied (False), this fails.
-- If the predicate hasn't been asked yet, this records a UserInputBlock
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
          let userInputBlock =
                UserInputBlock
                  { inputType = AskableInput,
                    name = predName,
                    question = question,
                    arguments = args,
                    candidates = [],
                    partialProof = partialProof
                  }
          modify' $ \s ->
            s {pendingUserInputs = userInputBlock : s.pendingUserInputs}
          mzero -- Fail this branch, allowing backtracking

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
      let (name, args) = fromMaybe ("proof", []) rule
      pure $ Just $ RuleApplied name args ps

-- | Append a proof step to the current proof stack.
appendProof :: Proof -> SolverM ()
appendProof proof = modify' $ \s ->
  s {proofStack = s.proofStack ++ [proof]}

--------------------------------------------------------------------------------
-- Committed Choice
--------------------------------------------------------------------------------

-- | Only try the second branch if the first completely fails.
--
-- Uses LogicT's 'ifte' for committed choice: if the first branch produces
-- any result, commit to it (don't backtrack into the second branch).
-- This prevents asking unnecessary questions when an earlier rule suffices.
orElse :: SolverM a -> SolverM a -> SolverM a
orElse a = ifte a pure

-- | Mutually exclusive conditional.
--
-- If the condition succeeds, combine its proof with the then-branch;
-- otherwise use the else-branch. The condition's proof is paired with
-- the then-branch proof via 'RuleApplied'.
ifThenElse :: SolverM Proof -> SolverM Proof -> SolverM Proof -> SolverM Proof
ifThenElse cond thenBranch elseBranch =
  ifte cond (\condProof -> do
    thenProof <- thenBranch
    pure $ RuleApplied "if_then_else" [] [condProof, thenProof]
  ) elseBranch

--------------------------------------------------------------------------------
-- Failure
--------------------------------------------------------------------------------

-- | Fail with an error message.
--
-- Records the failure reason in state for later diagnostic reporting,
-- then uses LogicT's mzero to backtrack and try alternatives.
failWith :: Text -> SolverM a
failWith msg = do
  rule <- gets (.currentRule)
  let ruleName = case rule of
        Nothing -> "unknown"
        Just (name, []) -> name
        Just (name, args) -> name <> "(" <> T.intercalate ", " (map scalarToText args) <> ")"
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

-- | Get failed proof paths from solver state.
getFailedPaths :: SolverState -> [FailurePath]
getFailedPaths s = s.failedPaths

--------------------------------------------------------------------------------
-- Tool Argument Extraction
--------------------------------------------------------------------------------

-- | Extract a scalar argument from tool arguments, failing if missing.
--
-- This is a convenience wrapper around 'extractToolArg' that fails in SolverM
-- when the argument is missing or invalid.
--
-- @
-- extractArg "bookingId" args >>= \\bookingId -> ...
-- @
extractArg :: Text -> Value -> SolverM Scalar
extractArg key args = case JSON.extractToolArg key args of
  Just s -> pure s
  Nothing -> failWith $ "Missing argument: " <> key

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

tshow :: (Show a) => a -> Text
tshow = fromString . show
