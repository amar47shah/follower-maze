-- | `Server`, a structure containing data that can be shared among
--
-- * one thread that reads from an event source
-- * many threads that each write notifications to a client.
--
-- `serveEventSource` and `serveUserClient` produce @IO ()@ actions
-- corresponding to these two tasks, given a `Server` and a connection handle.
module FollowerMaze.Server
  ( Server
  , initServer
  , serveEventSource
  , serveUserClient
  ) where

import FollowerMaze.Event
  ( Event (Message, Follow, Unfollow, Update, Broadcast)
  , RawEvent
  , UserId
  )
import FollowerMaze.EventQueue (EventQueue, dequeueAll, emptyQueue, enqueueRaw)
import FollowerMaze.Client (Client, clientUserId, beNotified, newClient, sendMessage)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Concurrent.STM.TVar
  ( TVar
  , newTVarIO
  , modifyTVar'
  , readTVar
  , writeTVar
  )
import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.STM (STM, atomically)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Safe (readMay)
import System.IO (Handle, hGetLine, hIsEOF)

-- | Comprises
--
-- * a priority queue of received events,
-- * a directory of connected clients, and
-- * a directory of followers for each user.
--
-- User identifiers function as keys in both directories.
data Server = Server
  { queue     :: EventQueue
  , clients   :: TVar (Map UserId Client)
  , followers :: TVar (Map UserId (Set UserId))
  }

-- | Initialize a server with an empty `EventQueue` and empty directories
-- for connected clients and user followers.
initServer :: IO Server
initServer = Server emptyQueue <$> newTVarIO Map.empty <*> newTVarIO Map.empty

-- | * receives a server and a connected client handle
--   * returns a process to write notifications to the client handle:
--
--   1. Read the client's user identifier.
--   2. If the user identifier is correctly formed and not already in use,
--      register the client in the server's directory of clients.
--   3. If the client connection was registered, receive notifications and
--      write them to the client.
--   4. When the client disconnects, remove it from the directory of clients.
serveUserClient :: Server -> Handle -> IO ()
serveUserClient server handle =
  bracket (acquireClient server handle) (releaseClient server) serveUserClient'
      where
  serveUserClient' :: Maybe Client -> IO ()
  serveUserClient' = maybe (pure ()) beNotified

-- | Internal only. Attempt to read a user identifier from the client connection,
-- and if successful, register the client.
acquireClient :: Server -> Handle -> IO (Maybe Client)
acquireClient server handle = do
  maybeUserId <- readMay <$> hGetLine handle
  maybe (pure Nothing) (registerClient server handle) maybeUserId

-- | Internal only. If the given user identifier is available, initialize a
-- `Client` and add an entry to the server's directory of clients.
registerClient :: Server -> Handle -> UserId -> IO (Maybe Client)
registerClient server handle userId = atomically $ do
  connections <- readTVar (clients server)
  if Map.member userId connections
  then pure Nothing
  else do
    client <- newClient userId handle
    writeTVar (clients server) $ Map.insert userId client connections
    pure $ Just client

-- | Internal only. Remove a client from the server's directory of clients.
releaseClient :: Server -> Maybe Client -> IO ()
releaseClient server = maybe (pure ()) (releaseClient' server)
      where
  releaseClient' :: Server -> Client -> IO ()
  releaseClient' s c =
    atomically . modifyTVar' (clients s) . Map.delete $ clientUserId c

-- | * receives a server and a connected event source handle
--   * returns a process to read events, route notifications to clients,
--     and update the directory of followers
--
-- Read events line by line until exhausting the events. For each line:
--
--   1. Enqueue the contents as an event in the server's queue.
--   2. Dequeue all ready events.
--   3. React to each dequeued event in sequence, according to the table.
--
-- @
-- | Event     | Referenced Users       | Change Server State | Notify                        |
-- |-----------|------------------------|---------------------|-------------------------------|
-- | Message   | sender    , recipient  | -                   | recipient, if connected       |
-- | Follow    | follower  , followed   | add follower        | followed user, if connected   |
-- | Unfollow  | unfollower, unfollowed | remove follower     | -                             |
-- | Update    | updater                | -                   | updater's connected followers |
-- | Broadcast | -                      | -                   | all connected clients         |
-- @
serveEventSource :: Server -> Handle -> IO ()
serveEventSource server handle = do
  newServer <- readLineAndProcess server handle
  isEOF <- hIsEOF handle
  if isEOF
  then pure ()
  else serveEventSource newServer handle

-- | Internal only. Read a raw event from the event source, enqueue it,
-- dequeue all ready events in sequence, and react as necessary.
--
-- Since enqueueing and dequeueing events results in a new queue,
-- return a new server containing the new queue.
readLineAndProcess :: Server -> Handle -> IO Server
readLineAndProcess server handle = do
  raw <- hGetLine handle
  let (events, newQueue) = dequeueAll $ enqueueRaw (queue server) raw
  let newServer = server { queue = newQueue }
  forM_ events $ atomically . react newServer
  pure newServer

-- | Internal only. Returns a composable STM action to process an event,
-- writing to the server's directory of followers and notifying clients as needed.
react :: Server -> Event -> STM ()
react s (Message   raw _    to) =                           notify    s raw to
react s (Follow    raw from to) = follow       s from to *> notify    s raw to
react s (Unfollow  _   from to) = unfollow     s from to
react s (Update    raw from   ) = getFollowers s from   >>= notifyAll s raw
react s (Broadcast raw        ) = allUsers     s        >>= notifyAll s raw

-- | Internal only. Returns an STM action to look up a user's followers in
-- the supplied server's directory.
getFollowers :: Server -> UserId -> STM (Set UserId)
getFollowers s f = Map.findWithDefault Set.empty f <$> readTVar (followers s)

-- | Internal only. Returns an STM action to produce the set of all connected
-- clients' user identifiers.
allUsers :: Server -> STM (Set UserId)
allUsers s = Map.keysSet <$> readTVar (clients s)

-- | Internal only. Returns an STM action to look up the given
-- user identifier in the supplied server, and if a connected client is found,
-- write the given notification to the client.
notify :: Server -> RawEvent -> UserId -> STM ()
notify s r t = readTVar (clients s)
           >>= maybe (pure ()) (sendMessage r) . Map.lookup t

-- | Internal only. Returns an STM action to write the given notification
-- to the given group of users, using the supplied server.
notifyAll :: Server -> RawEvent -> Set UserId -> STM ()
notifyAll s r ts = forM_ ts $ notify s r

-- | Internal only. Returns an STM action to modify the server's
-- directory of followers to add a follower.
follow ::
     Server
  -> UserId -- ^ follower
  -> UserId -- ^ followed
  -> STM ()
follow s f = modifyTVar' (followers s) . Map.alter (addFollower f)

-- | Internal only. Returns an STM action to modify the server's
-- directory of followers to remove a follower.
unfollow ::
     Server
  -> UserId -- ^ unfollower
  -> UserId -- ^ unfollowed
  -> STM ()
unfollow s f = modifyTVar' (followers s) . Map.alter (removeFollower f)

-- | Internal only. Receives an element and a `Maybe`-wrapped set of elements.
-- If the set is given, return it with the element inserted.
-- If not, return a singleton set containing the element.
addFollower :: Ord a => a -> Maybe (Set a) -> Maybe (Set a)
addFollower f = Just . Set.insert f . fromMaybe Set.empty

-- | Internal only. Receives an element and a `Maybe`-wrapped set of elements.
-- If the set is given, return it without the element, removing it if necessary.
-- If the set is not given, return `Nothing`.
removeFollower :: Ord a => a -> Maybe (Set a) -> Maybe (Set a)
removeFollower f mfs = Set.delete f <$> mfs
