-- | Module
module Client
  ( Client (clientUserId)
  , beNotified
  , newClient
  , sendMessage
  ) where

import Event (RawEvent, UserId)

import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Monad (forever)
import Control.Monad.STM (STM, atomically)
import System.IO (Handle, hPutStrLn)

-- | Exported
data Client = Client
  { clientUserId :: UserId
  , clientHandle :: Handle
  , clientChan   :: TChan RawEvent
  }

-- | Exported
beNotified :: Client -> IO ()
beNotified c = forever $ do
  msg <- atomically . readTChan $ clientChan c
  hPutStrLn (clientHandle c) msg

-- | Exported
newClient :: UserId -> Handle -> STM Client
newClient u h = Client u h <$> newTChan

-- | Exported
sendMessage :: RawEvent -> Client -> STM ()
sendMessage n c = writeTChan (clientChan c) n
