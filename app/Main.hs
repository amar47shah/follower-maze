module Main where

import Control.Monad (replicateM_)
import Network (PortID (PortNumber), accept, Socket, listenOn)
import System.IO (hClose, hGetLine)

main :: IO ()
main = do
  sockSource <- listen 9090
  (source, _, _) <- accept sockSource
  sockClient <- listen 9099
  replicateM_ 100 $ do
    (client, _, _) <- accept sockClient
    hClose client
  replicateM_ 10000000 $ hGetLine source >>= putStrLn
  hClose source

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral
