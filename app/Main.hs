module Main where

import Config (eventListenerPort, clientListenerPort)
import Server (initServer, serveEventSource, serveUserClient)

import Control.Concurrent (ThreadId, forkFinally)
import Control.Monad (forever, void)
import Network (PortID (PortNumber), Socket, accept, listenOn, withSocketsDo)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , hClose
  , hSetBuffering
  , hSetEncoding
  , utf8
  )

main :: IO ()
main = withSocketsDo $ do
  --
  -- Connect to source.
  sourceSocket <- listen eventListenerPort
  sourceHandle <- accept' sourceSocket
  configureHandle sourceHandle
  --
  server <- initServer
  --
  -- Fork a thread to receive and route events.
  void $ serveEventSource server `forkFinallyClosing` sourceHandle
  --
  -- Connect to clients.
  clientSocket <- listen clientListenerPort
  forever $ do
    clientHandle <- accept' clientSocket
    configureHandle clientHandle
    --
    -- Fork a thread to serve notifications to each client.
    serveUserClient server `forkFinallyClosing` clientHandle

-- | Internal only. Fork a thread to run an `IO` operation on a handle,
-- closing the handle when the thread is about to terminate, with an
-- exception or a returned value.
forkFinallyClosing :: (Handle -> IO a) -> Handle -> IO ThreadId
forkFinallyClosing process = forkFinally <$> process <*> const . hClose

accept' :: Socket -> IO Handle
accept' socket = (\(h, _, _) -> h) <$> accept socket

configureHandle :: Handle -> IO ()
configureHandle h = do
  hSetBuffering h LineBuffering
  hSetEncoding  h utf8

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral
