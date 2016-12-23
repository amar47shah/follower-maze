module Client (Client, beNotified, sendMessage, newClient) where

import Event (Notification, UserId)

import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Monad.STM (STM, atomically, orElse)
import System.IO (Handle, hPutStrLn)

data Client = Client
  { clientId     :: UserId
  , clientHandle :: Handle
  , clientChan   :: TChan Notification
  }

newClient :: UserId -> Handle -> STM Client
newClient u h = Client u h <$> newTChan

beNotified :: Client -> IO ()
beNotified c = do
  msg <- atomically . readTChan $ clientChan c
  hPutStrLn (clientHandle c) msg
  beNotified c

sendMessage :: Notification -> Client -> STM ()
sendMessage n c = writeTChan (clientChan c) n
