-- | Event sink and user input abstractions.
--
-- These abstractions decouple core Sentinel logic from specific I/O channels.
-- The console implementations use ANSI-styled putDocLn and blocking stdin
-- reads; a future frontend would send events over a websocket and receive
-- input via HTTP.
module Sentinel.Event
  ( EventSink (..),
    UserInput (..),
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

-- | A channel for obtaining side-session user input.
--
-- The @Text@ argument is the prompt string (e.g. @\"> \"@).
newtype UserInput = UserInput {getSideInput :: Text -> IO Text}

-- | Console event sink: renders to ANSI and prints to stdout.
consoleEventSink :: EventSink
consoleEventSink = EventSink putDocLn

-- | Console user input: prints the prompt, flushes stdout, reads a line.
consoleUserInput :: UserInput
consoleUserInput = UserInput \prompt -> do
  putStr (T.unpack prompt)
  hFlush stdout
  T.pack <$> getLine
