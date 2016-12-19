module Server (Server, initServer, processEvent, serveUser) where

import Event (Comm (Message, Follow, Unfollow, Update, Broadcast), Event (Event), Notification, UserId, parseEvent)
import Client (Client, newClient, beNotified, sendMessage)

import qualified Data.Map as Map
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad.STM (STM, atomically)
import Data.Map (Map)
import System.IO (Handle, hGetLine)

data Server = Server
  { connections   :: TVar (Map UserId Client)
  , broadcastChan :: TChan Notification
  }

initServer :: IO Server
initServer = Server <$> newTVarIO Map.empty <*> newBroadcastTChanIO

serveUser :: Server -> Handle -> IO ()
serveUser s h = getClient s h >>= beNotified

getClient :: Server -> Handle -> IO Client
getClient s h = do
  u <- hGetLine h
  atomically $ do
    client <- newClient u h $ broadcastChan s
    cs <- readTVar (connections s)
    writeTVar (connections s) $ Map.insert u client cs
    pure client

processEvent :: Server -> String -> IO ()
processEvent s = atomically . react s . parseEvent

react :: Server -> Event -> STM ()
react s (Event raw _ comm) =
  case comm of
    Broadcast        -> broadcast s raw
    Message  _    to -> message   s raw to
    Follow   _    _  -> pure ()
    Unfollow _    _  -> pure ()
    Update   _       -> pure ()

    where

  broadcast s r   = writeTChan (broadcastChan s) r
  message   s r t = do
    cs <- readTVar $ connections s
    maybe (pure ()) (sendMessage r) $ Map.lookup t cs
