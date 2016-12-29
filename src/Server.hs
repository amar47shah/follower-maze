-- | Module
module Server
  ( initServer
  , serveEventSource
  , serveUserClient
  ) where

import Event (Event (Broadcast, Follow, Message, Unfollow, Update), RawEvent, UserId)
import EventQueue (EventQueue, dequeueAll, emptyQueue, enqueueRaw)
import Client (Client, newClient, beNotified, sendMessage)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVar)
import Control.Monad (forM_)
import Control.Monad.STM (STM, atomically)
import Data.Map (Map)
import Data.Set (Set)
import System.IO (Handle, hGetLine, hIsEOF)

-- | Internal
data Server = Server
  { queue       :: EventQueue
  , connections :: TVar (Map UserId Client)
  , followers   :: TVar (Map UserId (Set UserId))
  }

-- | Exported
initServer :: IO Server
initServer = Server emptyQueue <$> newTVarIO Map.empty <*> newTVarIO Map.empty

-- | Exported
serveUserClient :: Server -> Handle -> IO ()
serveUserClient s h = getClient s h >>= beNotified

getClient :: Server -> Handle -> IO Client
getClient server handle = do
  userId <- read <$> hGetLine handle
  atomically $ do
    client <- newClient userId handle
    modifyTVar' (connections server) $ Map.insert userId client
    pure client

-- | Exported
serveEventSource :: Server -> Handle -> IO ()
serveEventSource server handle = do
  newServer <- readLineAndProcess server handle
  isEOF <- hIsEOF handle
  if isEOF
  then pure ()
  else serveEventSource newServer handle

readLineAndProcess :: Server -> Handle -> IO Server
readLineAndProcess server handle = do
  raw <- hGetLine handle
  let (events, newQueue) = dequeueAll $ enqueueRaw (queue server) raw
  let newServer = server { queue = newQueue }
  forM_ events $ atomically . react newServer
  pure newServer

react :: Server -> Event -> STM ()
react s (Message   raw _    to) =                           notify    s raw to
react s (Follow    raw from to) = follow       s from to *> notify    s raw to
react s (Unfollow  _   from to) = unfollow     s from to
react s (Update    raw from   ) = getFollowers s from   >>= notifyAll s raw
react s (Broadcast raw        ) = allUsers     s        >>= notifyAll s raw

getFollowers :: Server -> UserId -> STM (Set UserId)
getFollowers s f = Map.findWithDefault Set.empty f <$> readTVar (followers s)

allUsers :: Server -> STM (Set UserId)
allUsers s = Map.keysSet <$> readTVar (connections s)

notify :: Server -> RawEvent -> UserId -> STM ()
notify s r t = readTVar (connections s)
           >>= maybe (pure ()) (sendMessage r) . Map.lookup t

notifyAll :: Server -> RawEvent -> Set UserId -> STM ()
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
