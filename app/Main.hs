module Main where

import Config (eventListenerPort, clientListenerPort, concurrencyLevel, timeout)
import Server (Server, initServer, processEvent, serveUser)

import Control.Concurrent (ThreadId, forkFinally, threadDelay)
import Control.Monad (replicateM_, void)
import Network (PortID (PortNumber), accept, Socket, listenOn, withSocketsDo)
import System.IO (Handle, hClose, hGetLine, hIsEOF)

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
  void $ untilM hIsEOF (fmap <$> const <*> readAndProcess server) sourceHandle
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

readAndProcess :: Server -> Handle -> IO ()
readAndProcess s h = hGetLine h >>= processEvent s

untilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
untilM p k x =
  p x >>= \b ->
    if b
    then pure x
    else k x >>= untilM p k
