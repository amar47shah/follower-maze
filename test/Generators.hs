module Generators
  ( eventQueue
  , rawEvent
  , broadcast
  , follow
  , message
  , unfollow
  , update
  ) where

import Config (numberOfUsers, totalEvents)
import Event (RawEvent)
import EventQueue (EventQueue, emptyQueue, enqueueRaw)

import qualified Test.Tasty.QuickCheck as QC
import Control.Monad (replicateM)
import Data.List (intercalate)

-- | Exported
rawEvent :: QC.Gen RawEvent
rawEvent  = rawEvent'  =<< sequenceNumber

-- | Exported
follow, unfollow, broadcast, message, update :: QC.Gen RawEvent
follow    = follow'    =<< sequenceNumber
unfollow  = unfollow'  =<< sequenceNumber
broadcast = broadcast' =<< sequenceNumber
message   = message'   =<< sequenceNumber
update    = update'    =<< sequenceNumber

-- | Exported
eventQueue :: QC.Gen EventQueue
eventQueue = foldr (flip enqueueRaw) emptyQueue <$> rawEvents
      where
  rawEvents :: QC.Gen [RawEvent]
  rawEvents = sequenceA . fmap rawEvent' =<< lowSequenceNumbers
  lowSequenceNumbers :: QC.Gen [Integer]
  lowSequenceNumbers = QC.sublistOf =<< QC.shuffle [1..20]

rawEvent' :: Integer -> QC.Gen RawEvent
rawEvent' = QC.oneof . sequenceA [follow', unfollow', broadcast', message', update']

follow', unfollow', broadcast', message', update' :: Integer -> QC.Gen RawEvent
follow'    = rawEventGen "F" 2
unfollow'  = rawEventGen "U" 2
broadcast' = rawEventGen "B" 0
message'   = rawEventGen "P" 2
update'    = rawEventGen "S" 1

sequenceNumber :: QC.Gen Integer
sequenceNumber = QC.choose (1, toInteger totalEvents)

rawEventGen :: RawEvent -> Integer -> Integer -> QC.Gen RawEvent
rawEventGen code userCount sequenceN =
  intercalate "|" . ([show sequenceN, code] ++) <$> replicateM (fromIntegral userCount) user
      where
  user = show <$> QC.choose (1, numberOfUsers)
