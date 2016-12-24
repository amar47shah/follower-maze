module Main where

import Config (eventListenerPort, clientListenerPort, concurrencyLevel, timeout)
import Server (Server, initServer, serveEventSource, serveUserClient)

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Monad (replicateM_)
import Network (PortID (PortNumber), accept, Socket, listenOn, withSocketsDo)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hSetBuffering, hSetEncoding, utf8)

main :: IO ()
main = withSocketsDo $ do
  --
  -- Connect to source.
  sourceSocket <- listen eventListenerPort
  (sourceHandle, _, _) <- accept sourceSocket
  hSetBuffering sourceHandle LineBuffering
  hSetEncoding sourceHandle utf8
  --
  -- Initialize server.
  server <- initServer
  --
  -- Connect to clients and fork threads.
  clientSocket <- listen clientListenerPort
  replicateM_ concurrencyLevel $ do
    (handle, _, _) <- accept clientSocket
    forkUserThread server handle
  --
  -- Process events.
  serveEventSource server sourceHandle
  --
  -- Close source.
  hClose sourceHandle
  --
  -- Wait to deliver the rest of the notifications.
  threadDelay $ timeout * 100

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

forkUserThread :: Server -> Handle -> IO ThreadId
forkUserThread s = forkFinally <$> serveUserClient s <*> const . hClose
