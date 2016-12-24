module Main where

import Config (eventListenerPort, clientListenerPort, concurrencyLevel, timeout)
import Server (Server, initServer, readAndProcess, serveUser)

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Monad (replicateM_)
import Network (PortID (PortNumber), accept, Socket, listenOn, withSocketsDo)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hIsEOF, hSetBuffering, hSetEncoding, utf8)

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
  loop server sourceHandle
  --
  -- Close source.
  hClose sourceHandle
  --
  -- Wait to deliver the rest of the notifications.
  threadDelay $ timeout * 100 -- Tweak this value.

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

forkUserThread :: Server -> Handle -> IO ThreadId
forkUserThread s = forkFinally <$> serveUser s <*> const . hClose

loop :: Server -> Handle -> IO ()
loop s h = do
  newS <- readAndProcess s h
  isEOF <- hIsEOF h
  if isEOF
  then pure ()
  else loop newS h
