-- | `Client`,
-- a data structure representing a connection that receives notifications.
module FollowerMaze.Client
  ( Client
  , clientUserId
  , beNotified
  , newClient
  , sendNotification
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

-- | Given a `Client`, forever read notifications from its channel and
-- deliver them to its handle.
beNotified :: Client -> IO a
beNotified client = forever $
  deliver client =<< atomically (readClientChan client)

-- | Internal only. Write a notification to a client's handle.
deliver :: Client -> RawEvent -> IO ()
deliver = hPutStrLn . clientHandle

-- | Internal only. Read from the client's channel.
readClientChan :: Client -> STM RawEvent
readClientChan = readTChan . clientChan

-- | Returns an STM action to create a new `Client` with the
-- given user identifier and handle.
newClient :: UserId -> Handle -> STM Client
newClient u h = Client u h <$> newTChan

-- | Returns an STM action to write the a notification to a `Client`'s channel.
sendNotification :: RawEvent -> Client -> STM ()
sendNotification n c = writeTChan (clientChan c) n
