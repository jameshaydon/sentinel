-- | Event sink and user input abstractions.
--
-- These abstractions decouple core Sentinel logic from specific I/O channels.
-- The console implementations use ANSI-styled putDocLn and blocking stdin
-- reads; a future frontend would send events over a websocket and receive
-- input via HTTP.
module Sentinel.Event
  ( EventSink (..),
    UserInput (..),
    InputKind (..),
    InputMeta (..),
    consoleEventSink,
    consoleUserInput,
  )
where

import Data.Text qualified as T
import Pre
import System.IO (hFlush, stdout)

-- | An output channel for display events.
--
-- Events are pre-rendered to @Doc Ann@; the sink decides how to deliver them
-- (e.g. ANSI terminal, plain-text websocket frame, …).
newtype EventSink = EventSink {emit :: Doc Ann -> IO ()}

-- | The kind of user input being requested.
data InputKind
  = ContextInputKind
  | AskableInputKind
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Structured metadata for a user input request.
data InputMeta = InputMeta
  { question :: Text,
    inputKind :: InputKind,
    inputName :: Text,
    candidates :: [Text]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A channel for obtaining side-session user input.
--
-- The 'InputMeta' argument carries the question and metadata for the input widget.
newtype UserInput = UserInput {getSideInput :: InputMeta -> IO Text}

-- | Console event sink: renders to ANSI and prints to stdout.
consoleEventSink :: EventSink
consoleEventSink = EventSink putDocLn

-- | Console user input: extracts the question from 'InputMeta', prints it as a prompt,
-- flushes stdout, reads a line.
consoleUserInput :: UserInput
consoleUserInput = UserInput \meta -> do
  putStr (T.unpack meta.question <> "> ")
  hFlush stdout
  T.pack <$> getLine
