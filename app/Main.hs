module Main where

import qualified Data.Map as Map

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (replicateM_)
import Control.Monad.STM (STM, atomically)
import Data.List (unfoldr)
import Data.Map (Map)
import Network (PortID (PortNumber), accept, Socket, listenOn)
import System.IO (Handle, hClose, hGetLine, hPutStrLn)

main :: IO ()
main = do
  sourceSocket <- listen eventListenerPort
  (sourceHandle, _, _) <- accept sourceSocket
  server <- Server <$> newTVarIO Map.empty <*> newBroadcastTChanIO
  clientSocket <- listen clientListenerPort
  replicateM_ clientCount $ do
    (handle, _, _) <- accept clientSocket
    forkFinally (clientThread server handle) (const $ hClose handle)
  replicateM_ eventCount $ hGetLine sourceHandle >>= notify server . parseEvent
  hClose sourceHandle

clientCount, eventCount :: Int
clientCount = 10
eventCount  = 50000

clientListenerPort, eventListenerPort :: Int
clientListenerPort = 9099
eventListenerPort  = 9090

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

clientThread :: Server -> Handle -> IO ()
clientThread s h = getClient s h >>= beNotified

getClient :: Server -> Handle -> IO Client
getClient s h = do
  userId <- hGetLine h
  atomically $ do
    client <- newClient userId h s
    cs <- readTVar (clients s)
    writeTVar (clients s) $ Map.insert userId client cs
    pure client

beNotified :: Client -> IO ()
beNotified c = do
  msg <- atomically (readTChan $ clientBC c)
  hPutStrLn (clientHandle c) msg
  beNotified c

notify :: Server -> Event -> IO ()
notify s (Event r _ Broadcast) = atomically $ writeTChan (broadcast s) r
notify _ _                     = pure ()

type UserId = String
type Notification = String

data Server = Server
  { clients   :: TVar (Map UserId Client)
  , broadcast :: TChan Notification
  }

data Client = Client
  { clientId     :: UserId
  , clientHandle :: Handle
  , clientBC     :: TChan Notification
  }

newClient :: UserId -> Handle -> Server -> STM Client
newClient u h s = Client u h <$> dupTChan (broadcast s)

data Event = Event
  { eventRaw  :: Notification
  , eventSeq  :: Integer
  , eventComm :: Comm
  }

data Comm = Message  UserId UserId
          | Follow   UserId UserId
          | Unfollow UserId UserId
          | Update   UserId
          | Broadcast

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
