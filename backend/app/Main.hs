module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Examples.AirCanada.Facts qualified as AirCanada
import Examples.AirCanada.MockDB (initialDB)
import Examples.AirCanada.Sentinel (airCanadaSentinel)
import Examples.AirCanada.Tools (airCanadaSystemPrompt, airCanadaTools)
import Examples.AirCanada.Types (AirlineDB)
import Pre (Ann, Doc, putDocLn, renderDocPlain, vsep, wrappedText)
import Sentinel.Agent (AgentConfig (..), Message, defaultConfig, runAgent)
import Sentinel.Facts qualified as Facts
import Sentinel.LLM qualified as LLM
import Sentinel.Sentinel (Sentinel, SentinelEnv, newSentinelEnv)
import Sentinel.Tool (Tool)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (lookupEnv)
import Prelude

-- | Banner displayed at startup
bannerDoc :: Doc Ann
bannerDoc =
  vsep
    [ "=================================================",
      "   AIR CANADA AGENTIC CHATBOT (DEMO)            ",
      "=================================================",
      "",
      "Initializing...",
      ""
    ]

-- | Ready message with sample queries
readyDoc :: T.Text -> Doc Ann
readyDoc modelName =
  vsep
    [ "System Ready!",
      wrappedText ("Model: " <> modelName),
      "",
      "Sample queries you can try:",
      "  - \"What's the status of my booking REF123?\"",
      "  - \"Is flight AC102 delayed?\"",
      "  - \"I need a refund for booking REF789\"",
      "  - \"Find my bookings, my name is Alice Smith\"",
      "",
      "Type 'quit' or 'exit' to end the session.",
      "-------------------------------------------------"
    ]

-- | Error message for missing API key
missingKeyDoc :: Doc Ann
missingKeyDoc =
  vsep
    [ "ERROR: OPENAI_API_KEY environment variable not set.",
      "",
      "Please set your API key:",
      "  export OPENAI_API_KEY='your-key-here'",
      "",
      "You can get an API key from https://platform.openai.com/api-keys"
    ]

-- | Goodbye message
goodbyeDoc :: Doc Ann
goodbyeDoc = "Thank you for using Air Canada. Goodbye!"

-- | Agent response - the actual conversation output (not debug)
agentResponseDoc :: T.Text -> Doc Ann
agentResponseDoc response =
  vsep
    [ "",
      wrappedText response,
      ""
    ]

main :: IO ()
main = do
  putDocLn bannerDoc

  -- Get API key from environment
  maybeKey <- lookupEnv "OPENAI_API_KEY"
  case maybeKey of
    Nothing -> putDocLn missingKeyDoc
    Just apiKey -> do
      config <- defaultConfig (T.pack apiKey)
      let tools = airCanadaTools
          systemPrompt = airCanadaSystemPrompt
          sentinel = airCanadaSentinel
          db = initialDB
          initialFacts = Facts.emptyFacts

      -- Create the Sentinel environment with shared state
      sentinelEnv <- newSentinelEnv db initialFacts

      let modelName = T.pack (show (config.llmConfig.model :: LLM.Model))
      putDocLn (readyDoc modelName)

      repl config tools systemPrompt sentinel sentinelEnv [] 0

repl ::
  AgentConfig ->
  [Tool] ->
  T.Text ->
  Sentinel AirlineDB AirCanada.Fact ->
  SentinelEnv AirlineDB AirCanada.Fact ->
  [Message] ->
  Int ->
  IO ()
repl config tools systemPrompt sentinel sentinelEnv history turnCount =
  runInputT defaultSettings (loop history turnCount)
  where
    loop :: [Message] -> Int -> InputT IO ()
    loop hist currentTurnCount = do
      minput <- getInputLine "\n> "
      case minput of
        Nothing -> outputStrLn (T.unpack $ "\n" <> renderDocPlain goodbyeDoc)
        Just input
          | T.toLower (T.strip (T.pack input)) `elem` ["quit", "exit", "q"] ->
              outputStrLn (T.unpack $ "\n" <> renderDocPlain goodbyeDoc)
          | otherwise -> do
              (response, newHist, newTurnCount) <- liftIO $ runAgent config tools systemPrompt sentinel sentinelEnv hist currentTurnCount (T.pack input)
              outputStrLn (T.unpack $ "\n" <> renderDocPlain (agentResponseDoc response))
              loop newHist newTurnCount
