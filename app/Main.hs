module Main where

import Config (eventListenerPort, clientListenerPort)
import Server (initServer, serveEventSource, serveUserClient)

import Control.Concurrent (ThreadId, forkFinally)
import Control.Monad (forever, void)
import Network (PortID (PortNumber), accept, Socket, listenOn, withSocketsDo)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hSetBuffering, hSetEncoding, utf8)

main :: IO ()
main = withSocketsDo $ do
  --
  -- Connect to source.
  sourceSocket <- listen eventListenerPort
  (sourceHandle, _, _) <- accept sourceSocket
  --
  -- Process events.
  configureSourceHandle sourceHandle
  server <- initServer
  void $ serveEventSource server `forkFinallyClosing` sourceHandle
  --
  -- Connect to clients and fork threads.
  clientSocket <- listen clientListenerPort
  forever $ do
    (clientHandle, _, _) <- accept clientSocket
    serveUserClient server `forkFinallyClosing` clientHandle

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

configureSourceHandle :: Handle -> IO ()
configureSourceHandle h =
     hSetBuffering h LineBuffering
  *> hSetEncoding  h utf8

forkFinallyClosing :: (Handle -> IO a) -> Handle -> IO ThreadId
forkFinallyClosing process = forkFinally <$> process <*> const . hClose
