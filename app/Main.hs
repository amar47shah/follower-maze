module Main where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (replicateM_)
import Control.Monad.STM (atomically)
import Data.List (unfoldr)
import Network (PortID (PortNumber), accept, Socket, listenOn)
import System.IO (Handle, hClose, hGetLine)

main :: IO ()
main = do
  sourceSocket <- listen eventListenerPort
  (source, _, _) <- accept sourceSocket
  server <- Server <$> newTVarIO []
  clientSocket <- listen clientListenerPort
  replicateM_ clientCount $ do
    (client, _, _) <- accept clientSocket
    forkFinally (serve server client) (const $ hClose client)
  replicateM_ eventCount $ hGetLine source >>= print . parseEvent
  print =<< atomically (readTVar $ clients server)
  hClose source

clientCount, eventCount :: Int
clientCount = 10
eventCount  = 10

clientListenerPort, eventListenerPort :: Int
clientListenerPort = 9099
eventListenerPort  = 9090

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

serve :: Server -> Handle -> IO ()
serve s h = do
  c <- hGetLine h
  atomically $ do
    cs <- readTVar $ clients s
    writeTVar (clients s) $ c:cs

data Server = Server
  { clients :: TVar [User]
  }

data Event = Event
  { eventRaw  :: String
  , eventSeq  :: Integer
  , eventComm :: Comm
  } deriving Show

data Comm = Message  User User
          | Follow   User User
          | Unfollow User User
          | Update   User
          | Broadcast
          deriving Show

type User = String

parseEvent :: String -> Event
parseEvent = match <*> splitOn '|'
      where
  match :: String -> [String] -> Event
  match r (n:"P":from:to:[]) = Event r (read n) $ Message  from to
  match r (n:"F":from:to:[]) = Event r (read n) $ Follow   from to
  match r (n:"U":from:to:[]) = Event r (read n) $ Unfollow from to
  match r (n:"S":from   :[]) = Event r (read n) $ Update   from
  match r (n:"B"        :[]) = Event r (read n) $ Broadcast
  match _ _                  = error "Unrecognized event"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = unfoldr . splitOnceOn
      where
  splitOnceOn :: Eq a => a -> [a] -> Maybe ([a], [a])
  splitOnceOn _ [] = Nothing
  splitOnceOn d xs = Just . (drop 1 <$>) $ break (== d) xs
