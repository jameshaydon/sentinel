-- | WebSocket server interface for Sentinel.
--
-- Provides a WebSocket mode alongside the console REPL. The browser connects
-- via WebSocket, sends chat messages, and receives debug events + chat
-- responses as structured JSON. Single global session (no session management).
module Sentinel.WebSocket
  ( ServerMessage (..),
    ClientMessage (..),
    runWsServer,
  )
where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Network.WebSockets qualified as WS
import Pre
import Sentinel.Agent (AgentConfig, Message, runAgent)
import Sentinel.Example (Example (..), setupExample)
import Sentinel.Facts (BaseFactStore)
import Sentinel.Sentinel (EventSink (..), InputMeta, Sentinel, SentinelEnv, SessionData, UserInput (..), Verbosity)
import Sentinel.Toolkit (Toolkit)

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

-- | Messages sent from the server to the client.
data ServerMessage
  = -- | EventSink debug events (solver traces, tool calls, etc.)
    DebugEvent Text
  | -- | Agent final answer
    ChatResponse Text
  | -- | Side-session question prompting user input
    InputRequest InputMeta
  | -- | Session initialized with info text
    Ready Text
  | -- | Error message
    ServerError Text
  | -- | Fact store state update (sent whenever facts change)
    FactStoreUpdate BaseFactStore
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

-- | Messages sent from the client to the server.
data ClientMessage
  = -- | User sends a chat message
    UserChat Text
  | -- | User answers a side-session question
    InputResponse Text
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------
-- Per-Connection State
--------------------------------------------------------------------------------

-- | State for a single WebSocket connection.
data WsState = WsState
  { connection :: WS.Connection,
    chatChan :: TChan Text,
    inputVar :: MVar Text
  }

--------------------------------------------------------------------------------
-- EventSink / UserInput Implementations
--------------------------------------------------------------------------------

-- | Send a JSON message over the WebSocket connection.
sendMessage :: WS.Connection -> ServerMessage -> IO ()
sendMessage conn msg =
  WS.sendTextData conn (Aeson.encode msg)

-- | EventSink that renders Doc Ann to plain text and sends as DebugEvent.
wsEventSink :: WsState -> EventSink
wsEventSink ws = EventSink \doc ->
  sendMessage ws.connection (DebugEvent (renderDocPlain doc))

-- | UserInput that sends InputRequest and blocks waiting for InputResponse.
wsUserInput :: WsState -> UserInput
wsUserInput ws = UserInput \meta -> do
  sendMessage ws.connection (InputRequest meta)
  takeMVar ws.inputVar

--------------------------------------------------------------------------------
-- Receiver Thread
--------------------------------------------------------------------------------

-- | Receive loop: reads WebSocket frames, decodes ClientMessage, dispatches.
receiver :: WsState -> IO ()
receiver ws = go
  where
    go = do
      raw <- WS.receiveData ws.connection :: IO TL.Text
      case Aeson.eitherDecode (TL.encodeUtf8 raw) of
        Left err ->
          sendMessage ws.connection (ServerError (T.pack err)) >> go
        Right (UserChat text) -> do
          atomically (writeTChan ws.chatChan text)
          go
        Right (InputResponse answer) -> do
          putMVar ws.inputVar answer
          go

--------------------------------------------------------------------------------
-- Main Loop
--------------------------------------------------------------------------------

-- | Main chat loop: reads from chatChan, runs agent, sends response.
mainLoop ::
  AgentConfig ->
  Text ->
  Sentinel db ->
  SentinelEnv db ->
  Toolkit db ->
  WsState ->
  [Message] ->
  Int ->
  IO ()
mainLoop config sysPrompt sentinel sentinelEnv toolkit ws history turnCount = do
  userText <- atomically (readTChan ws.chatChan)
  (response, newHistory, newTurnCount) <-
    runAgent config toolkit sysPrompt sentinel sentinelEnv history turnCount userText
  sendMessage ws.connection (ChatResponse response)
  mainLoop config sysPrompt sentinel sentinelEnv toolkit ws newHistory newTurnCount

--------------------------------------------------------------------------------
-- Server Entry Point
--------------------------------------------------------------------------------

-- | Run the WebSocket server.
runWsServer :: Int -> AgentConfig -> Example db -> SessionData -> Verbosity -> IO ()
runWsServer port config ex sessionData verbosityLevel = do
  putDocLn $ "Starting WebSocket server on port " <> pretty port
  WS.runServer "0.0.0.0" port (application config ex sessionData verbosityLevel)

-- | WebSocket application handler for a single connection.
application :: AgentConfig -> Example db -> SessionData -> Verbosity -> WS.ServerApp
application config ex sessionData verbosityLevel pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (pure ()) do
    chatChan <- newTChanIO
    inputVar <- newEmptyMVar
    let ws = WsState {connection = conn, chatChan, inputVar}
        sink = wsEventSink ws
        input = wsUserInput ws

    let factSink store = sendMessage conn (FactStoreUpdate store)

    (sysPrompt, sentinel, sentinelEnv, toolkit) <-
      setupExample ex sessionData verbosityLevel sink input factSink

    sendMessage conn (Ready ("Connected to " <> ex.name <> " example"))

    withAsync (receiver ws) \_ ->
      mainLoop config sysPrompt sentinel sentinelEnv toolkit ws [] 0
