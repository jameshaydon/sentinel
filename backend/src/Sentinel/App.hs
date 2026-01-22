-- | Application entry point for sentinel CLI.
module Sentinel.App
  ( main,
  )
where

import Data.Text qualified as T
import Examples.AirCanada.Example (airCanadaExample)
import Examples.AirLogic.Example (airLogicExample)
import Pre (Ann, Doc, indent, pretty, putDocLn, vsep, wrappedText, (<+>))
import Sentinel.Agent (AgentConfig (..), defaultConfig)
import Sentinel.Example (Example (..), runExample)
import Sentinel.LLM qualified as LLM
import Sentinel.Sentinel (SessionData (..))
import System.Environment (getArgs, lookupEnv)
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

-- | Usage message. Takes list of (command, description) pairs.
usageDoc :: [(String, String)] -> Doc Ann
usageDoc examples =
  vsep
    [ "Usage: sentinel <example> --user <user_id>",
      "",
      "Arguments:",
      indent 2 $ vsep ["<example>     - The example to run", "--user <id>   - The user ID (required)"],
      "",
      "Available examples:",
      indent 2 $ vsep (fmap exampleLine examples),
      "",
      exampleHint
    ]
  where
    exampleLine :: (String, String) -> Doc Ann
    exampleLine (cmd, desc) = pretty cmd <+> "-" <+> pretty desc
    exampleHint = case examples of
      ((cmd, _) : _) -> "Example: sentinel " <> pretty cmd <> " --user usr_sarah_chen"
      [] -> ""

-- | Error message for missing --user flag.
missingUserDoc :: Doc Ann
missingUserDoc =
  vsep
    [ "ERROR: --user <user_id> is required.",
      "",
      "Please specify a user ID to run the example:",
      "  sentinel airlogic --user usr_sarah_chen"
    ]

-- | Run with a specific example and user ID.
runWithExample :: Example db -> T.Text -> IO ()
runWithExample ex userId = do
  putDocLn (bannerDoc ex)
  maybeKey <- lookupEnv "OPENAI_API_KEY"
  case maybeKey of
    Nothing -> putDocLn missingKeyDoc
    Just apiKey -> do
      config <- defaultConfig (T.pack apiKey)
      let modelName = T.pack (show (config.llmConfig.model :: LLM.Model))
          sessionData = SessionData {userId = Just userId}
      putDocLn (readyDoc modelName ex)
      runExample config ex sessionData

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["airlogic", "--user", userId] -> runWithExample airLogicExample (T.pack userId)
    ["aircanada", "--user", userId] -> runWithExample airCanadaExample (T.pack userId)
    ["airlogic"] -> putDocLn missingUserDoc
    ["aircanada"] -> putDocLn missingUserDoc
    [other] -> do
      putDocLn $ "Unknown example: " <> pretty other
      putDocLn ""
      putDocLn usage
    _ -> putDocLn usage
  where
    usage = usageDoc [("airlogic", "AirLogic Airlines Customer Service"), ("aircanada", "Air Canada Customer Service")]
