-- | Module
module Client
  ( Client
  , beNotified
  , newClient
  , sendMessage
  ) where

import Event (RawEvent, UserId)

import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Monad.STM (STM, atomically)
import System.IO (Handle, hPutStrLn)

-- | Exported
data Client = Client UserId Handle (TChan RawEvent)

handle :: Client -> Handle
handle (Client _ h _) = h

chan :: Client -> TChan RawEvent
chan (Client _ _ c) = c

-- | Exported
beNotified :: Client -> IO ()
beNotified c = do
  msg <- atomically . readTChan $ chan c
  hPutStrLn (handle c) msg
  beNotified c

-- | Exported
newClient :: UserId -> Handle -> STM Client
newClient u h = Client u h <$> newTChan

-- | Exported
sendMessage :: RawEvent -> Client -> STM ()
sendMessage n c = writeTChan (chan c) n
