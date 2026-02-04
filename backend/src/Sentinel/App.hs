-- | Application entry point for sentinel CLI.
module Sentinel.App
  ( main,
  )
where

import Data.Text qualified as T
import Examples.AirCanada.Example (airCanadaExample)
import Examples.AirLogic.Example (airLogicExample)
import Examples.Passport.Example (passportExample)
import Options.Generic (ParseRecord, Unwrapped, Wrapped, unwrapRecord, type (:::), type (<?>))
import Pre (Ann, Doc, Generic, indent, pretty, putDocLn, vsep, wrappedText)
import Sentinel.Agent (AgentConfig (..), defaultConfig)
import Sentinel.Example (Example (..), runExample)
import Sentinel.LLM qualified as LLM
import Sentinel.Sentinel (SessionData (..), Verbosity (..), consoleEventSink, consoleUserInput)
import Sentinel.WebSocket qualified as WebSocket
import System.Environment (lookupEnv)
import Prelude

-- | Banner displayed at startup.
bannerDoc :: Example db -> Doc Ann
bannerDoc ex =
  vsep
    [ "=================================================",
      "  " <> pretty ex.description <> " (DEMO)",
      "=================================================",
      "",
      "Initializing...",
      ""
    ]

-- | Ready message with sample queries.
readyDoc :: T.Text -> Example db -> Doc Ann
readyDoc modelName ex =
  vsep
    [ "System Ready!",
      wrappedText ("Model: " <> modelName),
      "",
      "Sample queries you can try:",
      indent 2 $ vsep (fmap (\q -> "- \"" <> pretty q <> "\"") ex.sampleQueries),
      "",
      "Type 'quit' or 'exit' to end the session.",
      "-------------------------------------------------"
    ]

-- | Error message for missing API key.
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

-- | CLI options for the sentinel executable.
data Options w = Options
  { example :: w ::: T.Text <?> "Example to run (airlogic, aircanada, passport)",
    user :: w ::: T.Text <?> "User ID for the session",
    verbosity :: w ::: Maybe Verbosity <?> "Verbosity level (silent, basic, detailed, verbose)",
    mode :: w ::: Maybe T.Text <?> "Mode: console (default) or websocket",
    port :: w ::: Maybe Int <?> "WebSocket port (default 8080, only used in websocket mode)"
  }
  deriving stock (Generic)

instance ParseRecord (Options Wrapped)

deriving stock instance Show (Options Unwrapped)

-- | Run with a specific example and user ID.
runWithExample :: Example db -> T.Text -> Verbosity -> T.Text -> Int -> IO ()
runWithExample ex userId verbosityLevel runMode wsPort = do
  putDocLn (bannerDoc ex)
  maybeKey <- lookupEnv "OPENAI_API_KEY"
  case maybeKey of
    Nothing -> putDocLn missingKeyDoc
    Just apiKey -> do
      config <- defaultConfig (T.pack apiKey)
      let sessionData = SessionData {userId = Just userId}
      case runMode of
        "websocket" ->
          WebSocket.runWsServer wsPort config ex sessionData verbosityLevel
        _ -> do
          let modelName = T.pack (show (config.llmConfig.model :: LLM.Model))
          putDocLn (readyDoc modelName ex)
          runExample config ex sessionData verbosityLevel consoleEventSink consoleUserInput

-- | Main entry point.
main :: IO ()
main = do
  opts :: Options Unwrapped <- unwrapRecord "Sentinel - Governance middleware for LLM agents"
  let verbosityLevel = maybe Silent id opts.verbosity
      runMode = maybe "console" id opts.mode
      wsPort = maybe 8080 id opts.port
  case opts.example of
    "airlogic" -> runWithExample airLogicExample opts.user verbosityLevel runMode wsPort
    "aircanada" -> runWithExample airCanadaExample opts.user verbosityLevel runMode wsPort
    "passport" -> runWithExample passportExample opts.user verbosityLevel runMode wsPort
    other -> do
      putDocLn $ "Unknown example: " <> pretty other
      putDocLn ""
      putDocLn "Available examples: airlogic, aircanada, passport"
