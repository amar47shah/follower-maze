module Main where

import Config (eventListenerPort, clientListenerPort, totalEvents, concurrencyLevel)
import Server (Server, initServer, processEvent, serveUser)

import Control.Concurrent (ThreadId, forkFinally)
import Control.Monad (replicateM_)
import Network (PortID (PortNumber), accept, Socket, listenOn, withSocketsDo)
import System.IO (Handle, hClose, hGetLine)

main :: IO ()
main = withSocketsDo $ do
  --
  -- Connect to source.
  sourceSocket <- listen eventListenerPort
  (sourceHandle, _, _) <- accept sourceSocket
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
  replicateM_ totalEvents $ hGetLine sourceHandle >>= processEvent server
  --
  -- Close source.
  hClose sourceHandle

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

forkUserThread :: Server -> Handle -> IO ThreadId
forkUserThread s = forkFinally <$> serveUser s <*> const . hClose
