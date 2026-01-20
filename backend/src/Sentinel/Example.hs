-- | Example packaging for sentinel demos.
--
-- This module provides the 'Example' type that bundles everything needed
-- to run an example: toolkit, database, welcome messages, and sample queries.
module Sentinel.Example
  ( Example (..),
    runExample,
  )
where

import Data.Text qualified as T
import Pre
import Sentinel.Agent (AgentConfig (..), Message, runAgent)
import Sentinel.Facts qualified as Facts
import Sentinel.Sentinel (Sentinel, SentinelEnv, newSentinelEnv)
import Sentinel.Tool (LLMTool)
import Sentinel.Toolkit (Toolkit (..), toLLMTools, toolkitSentinel)
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

-- | Run an example with the given agent configuration.
runExample :: AgentConfig -> Example db -> IO ()
runExample config ex = do
  let tools = toLLMTools ex.toolkit
      sysPrompt = "Be terse and concise in your responses. This is a demo/prototype.\n\n" <> ex.toolkit.systemPrompt
      sentinel = toolkitSentinel ex.toolkit
      facts = Facts.emptyBaseFactStore

  sentinelEnv <- newSentinelEnv ex.initialDB facts
  repl config tools sysPrompt sentinel sentinelEnv ex.goodbyeMessage [] 0

repl ::
  AgentConfig ->
  [LLMTool] ->
  T.Text ->
  Sentinel db ->
  SentinelEnv db ->
  Text ->
  [Message] ->
  Int ->
  IO ()
repl config tools systemPrompt sentinel sentinelEnv goodbye history turnCount =
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
          | otherwise -> do
              (response, newHist, newTurnCount) <-
                liftIO $ runAgent config tools systemPrompt sentinel sentinelEnv hist currentTurnCount (T.pack input)
              outputStrLn (T.unpack $ "\n" <> response <> "\n")
              loop newHist newTurnCount
