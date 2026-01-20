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
    [ "Usage: sentinel <example>",
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
      ((cmd, _) : _) -> "Example: sentinel " <> pretty cmd
      [] -> ""

-- | Run with a specific example.
runWithExample :: Example db -> IO ()
runWithExample ex = do
  putDocLn (bannerDoc ex)
  maybeKey <- lookupEnv "OPENAI_API_KEY"
  case maybeKey of
    Nothing -> putDocLn missingKeyDoc
    Just apiKey -> do
      config <- defaultConfig (T.pack apiKey)
      let modelName = T.pack (show (config.llmConfig.model :: LLM.Model))
      putDocLn (readyDoc modelName ex)
      runExample config ex

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["airlogic"] -> runWithExample airLogicExample
    ["aircanada"] -> runWithExample airCanadaExample
    [other] -> do
      putDocLn $ "Unknown example: " <> pretty other
      putDocLn ""
      putDocLn usage
    _ -> putDocLn usage
  where
    usage = usageDoc [("airlogic", "AirLogic Airlines Customer Service"), ("aircanada", "Air Canada Customer Service")]
