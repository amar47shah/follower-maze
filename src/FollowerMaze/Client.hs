-- | `Client`,
-- a data structure representing a connection that receives notifications.
module FollowerMaze.Client
  ( Client
  , clientUserId
  , beNotified
  , newClient
  , sendMessage
  ) where

import FollowerMaze.Event (RawEvent, UserId)

import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.STM (STM, atomically)
import System.IO (Handle, hPutStrLn)

-- | Comprises a connected client's
--
-- * user identifier
-- * connection handle, and
-- * channel for receiving notifications.
data Client = Client
  { clientUserId :: UserId         -- ^ Return the `Client`'s user identifier.
  , clientHandle :: Handle
  , clientChan   :: TChan RawEvent
  }

-- | Given a `Client`, forever read messages from its message channel and
-- write them to its handle.
beNotified :: Client -> IO a
beNotified c = forever $ do
  msg <- atomically . readTChan $ clientChan c
  hPutStrLn (clientHandle c) msg

-- | Returns an STM action to create a new `Client` with the
-- given user identifier and handle.
newClient :: UserId -> Handle -> STM Client
newClient u h = Client u h <$> newTChan

-- | Returns an STM action to write the given notification to the
-- given `Client`'s message channel.
sendMessage :: RawEvent -> Client -> STM ()
sendMessage n c = writeTChan (clientChan c) n
