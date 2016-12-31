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
import EventQueue (EventQueue, emptyQueue, newQueueAt, enqueueRaw)

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

rawEvent' :: Integer -> QC.Gen RawEvent
rawEvent' =
  QC.oneof . sequenceA [follow', unfollow', broadcast', message', update']

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
  intercalate "|" . ([show sequenceN, code] ++) <$> users
      where
  users = replicateM (fromIntegral userCount) user
  user  = show <$> QC.choose (1, numberOfUsers)

-- | Exported
-- A loaded eventQueue.
eventQueue :: QC.Gen EventQueue
eventQueue = QC.frequency
  [ (9, correct)
  , (3, offset)
  , (3, havingDuplicates)
  , (1, offsetHavingDuplicates)
  ]
      where
  correct, havingDuplicates, offset, offsetHavingDuplicates :: QC.Gen EventQueue
  correct                = eventQueue' alignedNewQueue withoutDuplicates
  havingDuplicates       = eventQueue' alignedNewQueue withDuplicates
  offset                 = eventQueue' offsetNewQueue  withoutDuplicates
  offsetHavingDuplicates = eventQueue' offsetNewQueue  withDuplicates

eventQueue' :: QC.Gen EventQueue -> QC.Gen [Integer] -> QC.Gen EventQueue
eventQueue' initial inputNums =
  foldr (flip enqueueRaw) <$> initial <*> rawEventsFromSequenceNumbers inputNums
      where
  rawEventsFromSequenceNumbers :: QC.Gen [Integer] -> QC.Gen [RawEvent]
  rawEventsFromSequenceNumbers ns = ns >>= sequenceA . fmap rawEvent'

alignedNewQueue :: QC.Gen EventQueue
alignedNewQueue = pure emptyQueue

offsetNewQueue :: QC.Gen EventQueue
offsetNewQueue = newQueueAt <$> QC.choose (2, smallNum)

withoutDuplicates :: QC.Gen [Integer]
withoutDuplicates = QC.sublistOf =<< QC.shuffle [1..smallNum]

withDuplicates :: QC.Gen [Integer]
withDuplicates = QC.listOf $ QC.choose (1, smallNum)

smallNum :: Integer
smallNum = 20
