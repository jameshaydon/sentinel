-- | Example packaging for sentinel demos.
--
-- This module provides the 'Example' type that bundles everything needed
-- to run an example: toolkit, database, welcome messages, and sample queries.
module Sentinel.Example
  ( Example (..),
    runExample,
    setupExample,
  )
where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Pre
import Sentinel.Agent (AgentConfig (..), Message, runAgent)
import Sentinel.Context (ContextEstablishment (..), ContextStore (..), EstablishmentMethod (..))
import Sentinel.Facts qualified as Facts
import Sentinel.Output qualified as Output
import Sentinel.Facts (BaseFactStore)
import Sentinel.Sentinel (EventSink (..), Sentinel, SentinelEnv (..), SessionData, UserInput, Verbosity (..), getContextStore, newSentinelEnv, runSentinelM, setVerbosity)
import Sentinel.Solver.Types (scalarToText)
import Sentinel.Toolkit (Toolkit (..), toolkitSentinel)
import Sentinel.Verbosity (parseVerbosity)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

-- | An example packages everything needed to run a sentinel demo.
data Example db = Example
  { name :: Text,
    description :: Text,
    sampleQueries :: [Text],
    goodbyeMessage :: Text,
    toolkit :: Toolkit db,
    initialDB :: db
  }

-- | Set up an example, returning the components needed to run it.
--
-- Both the console REPL and WebSocket server use this to initialize a session.
setupExample ::
  Example db ->
  SessionData ->
  Verbosity ->
  EventSink ->
  UserInput ->
  (BaseFactStore -> IO ()) ->
  IO (Text, Sentinel db, SentinelEnv db, Toolkit db)
setupExample ex sessionData verbosityLevel sink input factSink = do
  let sysPrompt = "Be terse and concise in your responses. This is a demo/prototype.\n\n" <> ex.toolkit.systemPrompt
      sentinel = toolkitSentinel ex.toolkit
      facts = Facts.emptyBaseFactStore

  sentinelEnv <- newSentinelEnv ex.initialDB facts sessionData ex.toolkit.contextDecls verbosityLevel sink input factSink

  -- Display seeded context
  ctxStore <- runSentinelM sentinelEnv getContextStore
  let seededCtx = getSeededContext ctxStore
  sentinelEnv.eventSink.emit (disp (Output.SessionInfo seededCtx))

  pure (sysPrompt, sentinel, sentinelEnv, ex.toolkit)

-- | Run an example with the given agent configuration and session data.
runExample :: AgentConfig -> Example db -> SessionData -> Verbosity -> EventSink -> UserInput -> IO ()
runExample config ex sessionData verbosityLevel sink input = do
  (sysPrompt, sentinel, sentinelEnv, toolkit) <- setupExample ex sessionData verbosityLevel sink input (const (pure ()))
  repl config toolkit sysPrompt sentinel sentinelEnv ex.goodbyeMessage [] 0

-- | Extract seeded context variables from the context store.
-- Returns only SystemSeeded values as (slot name, value text) pairs.
getSeededContext :: ContextStore -> [(Text, Text)]
getSeededContext (ContextStore established) =
  [ (slot, scalarToText est.value)
  | (slot, est) <- M.toList established,
    isSystemSeeded est.establishedVia
  ]
  where
    isSystemSeeded SystemSeeded = True
    isSystemSeeded _ = False

repl ::
  AgentConfig ->
  Toolkit db ->
  T.Text ->
  Sentinel db ->
  SentinelEnv db ->
  Text ->
  [Message] ->
  Int ->
  IO ()
repl config toolkit systemPrompt sentinel sentinelEnv goodbye history turnCount =
  runInputT defaultSettings (loop history turnCount)
  where
    loop :: [Message] -> Int -> InputT IO ()
    loop hist currentTurnCount = do
      minput <- getInputLine "\n> "
      case minput of
        Nothing -> outputStrLn (T.unpack $ "\n" <> goodbye)
        Just input
          | T.toLower (T.strip (T.pack input)) `elem` ["quit", "exit", "q"] ->
              outputStrLn (T.unpack $ "\n" <> goodbye)
          | "/debug " `T.isPrefixOf` T.pack input -> do
              let levelStr = T.strip $ T.drop 7 (T.pack input)
              case parseVerbosity (T.unpack levelStr) of
                Just level -> do
                  liftIO $ runSentinelM sentinelEnv (setVerbosity level)
                  outputStrLn $ "Debug level set to: " <> show level
                  loop hist currentTurnCount
                Nothing -> do
                  outputStrLn $ "Invalid debug level: " <> T.unpack levelStr
                  outputStrLn "Valid levels: silent, basic, detailed, verbose"
                  loop hist currentTurnCount
          | otherwise -> do
              (response, newHist, newTurnCount) <-
                liftIO $ runAgent config toolkit systemPrompt sentinel sentinelEnv hist currentTurnCount (T.pack input)
              outputStrLn (T.unpack $ "\n" <> response <> "\n")
              loop newHist newTurnCount
