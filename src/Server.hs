module Server (Server, initServer, processEvent, serveUser) where

import Event (Comm (Message, Follow, Unfollow, Update, Broadcast), Event (Event), Notification, UserId, parseEvent)
import Client (Client, newClient, beNotified, sendMessage)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVar)
import Control.Monad (forM_)
import Control.Monad.STM (STM, atomically)
import Data.Map (Map)
import Data.Set (Set)
import System.IO (Handle, hGetLine)

data Server = Server
  { connections :: TVar (Map UserId Client)
  , followers   :: TVar (Map UserId (Set UserId))
  }

initServer :: IO Server
initServer = Server <$> newTVarIO Map.empty <*> newTVarIO Map.empty

serveUser :: Server -> Handle -> IO ()
serveUser s h = getClient s h >>= beNotified

getClient :: Server -> Handle -> IO Client
getClient s h = do
  u <- read <$> hGetLine h
  atomically $ do
    client <- newClient u h
    modifyTVar' (connections s) $ Map.insert u client
    pure client

processEvent :: Server -> String -> IO ()
processEvent s = atomically . react s . parseEvent

react :: Server -> Event -> STM ()
react serv (Event raw _ comm) =
  case comm of
    Message  _    to ->                              notify    serv raw to
    Follow   from to -> follow       serv from to *> notify    serv raw to
    Unfollow from to -> unfollow     serv from to
    Update   from    -> getFollowers serv from   >>= notifyAll serv raw
    Broadcast        -> allUsers     serv        >>= notifyAll serv raw

getFollowers :: Server -> UserId -> STM (Set UserId)
getFollowers s f = Map.findWithDefault Set.empty f <$> readTVar (followers s)

allUsers :: Server -> STM (Set UserId)
allUsers s = Map.keysSet <$> readTVar (connections s)

notify :: Server -> Notification -> UserId -> STM ()
notify s r t = readTVar (connections s)
           >>= maybe (pure ()) (sendMessage r) . Map.lookup t

notifyAll :: Server -> Notification -> Set UserId -> STM ()
notifyAll s r ts = forM_ ts $ notify s r

follow :: Server -> UserId -> UserId -> STM ()
follow s f = modifyTVar' (followers s) . Map.alter (addFollower f)

unfollow :: Server -> UserId -> UserId -> STM ()
unfollow s f = modifyTVar' (followers s) . Map.alter (removeFollower f)

addFollower :: Ord a => a -> Maybe (Set a) -> Maybe (Set a)
addFollower f Nothing   = Just $ Set.insert f Set.empty
addFollower f (Just fs) = Just $ Set.insert f fs

removeFollower :: Ord a => a -> Maybe (Set a) -> Maybe (Set a)
removeFollower f mfs = Set.delete f <$> mfs
