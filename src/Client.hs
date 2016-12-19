module Client (Client, beNotified, sendMessage, newClient) where

import Event (Notification, UserId)

import Control.Concurrent.STM.TChan (TChan, dupTChan, newTChan, readTChan, writeTChan)
import Control.Monad.STM (STM, atomically, orElse)
import System.IO (Handle, hPutStrLn)

data Client = Client
  { clientId     :: UserId
  , clientHandle :: Handle
  , clientDM     :: TChan Notification
  , clientBC     :: TChan Notification
  }

newClient :: UserId -> Handle -> TChan Notification -> STM Client
newClient u h c = Client u h <$> newTChan <*> dupTChan c

beNotified :: Client -> IO ()
beNotified c = do
  msg <- atomically $ readTChan (clientBC c) `orElse` readTChan (clientDM c)
  hPutStrLn (clientHandle c) msg
  beNotified c

sendMessage :: Notification -> Client -> STM ()
sendMessage n c = writeTChan (clientDM c) n
