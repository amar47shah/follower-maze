-- | Random QuickCheck generators for loaded `eventQueue`s and correctly formed
-- `rawEvent`s. Generators are provided for specific raw events:
-- `message`, `follow`, `unfollow`, `update`, and `broadcast`.
module FollowerMaze.Test.Generators
  ( eventQueue
  , rawEvent
  , message
  , follow
  , unfollow
  , update
  , broadcast
  ) where

import FollowerMaze.Config (numberOfUsers, totalEvents)
import FollowerMaze.Event (RawEvent, SequenceNumber)
import FollowerMaze.EventQueue (EventQueue, emptyQueue, enqueueRaw, newQueueAt)

import qualified Test.Tasty.QuickCheck as QC
import Control.Monad (replicateM)
import Data.List (intercalate)

-- | Generates a correctly formed raw event.
rawEvent :: QC.Gen RawEvent
rawEvent  = rawEvent'  =<< sequenceNumber

-- | Generates a correctly formed raw event of a particular type.
message, follow, unfollow, update, broadcast :: QC.Gen RawEvent
message   = message'   =<< sequenceNumber
follow    = follow'    =<< sequenceNumber
unfollow  = unfollow'  =<< sequenceNumber
update    = update'    =<< sequenceNumber
broadcast = broadcast' =<< sequenceNumber

-- | Internal only. Returns a generator for raw events
-- with the given sequence number.
rawEvent' :: SequenceNumber -> QC.Gen RawEvent
rawEvent' =
  QC.oneof . sequenceA [follow', unfollow', broadcast', message', update']

-- | Internal only. Returns a generator for raw events of a particular type
-- with the given sequence number.
message', follow', unfollow', update', broadcast' ::
  SequenceNumber -> QC.Gen RawEvent
message'   = rawEventGen "P" 2
follow'    = rawEventGen "F" 2
unfollow'  = rawEventGen "U" 2
update'    = rawEventGen "S" 1
broadcast' = rawEventGen "B" 0

-- | Internal only. Generates a sequence number between @1@ and the value
-- of `FollowerMaze.Config.totalEvents`.
sequenceNumber :: QC.Gen SequenceNumber
sequenceNumber = QC.choose (1, toInteger totalEvents)

-- | Internal only. Returns a generator for raw events meeting the given
-- specifications. Most combinations of event code and user id count
-- result in generators that produce invalid, unparseable raw events.
rawEventGen ::
     RawEvent        -- ^ event code
  -> Int             -- ^ user id count
  -> SequenceNumber
  -> QC.Gen RawEvent
rawEventGen code userCount sequenceN =
  intercalate "|" . ([show sequenceN, code] ++) <$> replicateM userCount user
      where
  user = show <$> QC.choose (1, numberOfUsers)

-- | Generates an `EventQueue` with 0 or more raw events enqueued
-- using the `enqueueRaw` operation.
--
-- The following cases are generated in a weighted (9:3:3:1) distribution:
--
-- * a correctly loaded queue containing events with no duplicate
--   sequence numbers, and set to dequeue events with sequence numbers
--   less than or equal to @1@.
-- * an "offset" queue containing no duplicate events, but possibly
--   containing events with smaller sequence numbers than its next expected
--   sequence number (which is larger than @1@). That is, an offset queue
--   may contain events that it __should__ have dequeued already.
-- * a non-offset queue that may contain events with duplicate sequence numbers.
-- * an offset queue that may contain events with duplicate sequence numbers.
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

-- | Internal only. Returns a generator for `EventQueue`s, given a generator for
-- initial queues and a generator for sequence number collections.
eventQueue' :: QC.Gen EventQueue -> QC.Gen [SequenceNumber] -> QC.Gen EventQueue
eventQueue' initial inputNums =
  foldr (flip enqueueRaw) <$> initial <*> rawEventsFromSequenceNumbers inputNums
      where
  -- | Returns a generator for raw event collections, given a generator
  -- for sequence number collections.
  rawEventsFromSequenceNumbers :: QC.Gen [SequenceNumber] -> QC.Gen [RawEvent]
  rawEventsFromSequenceNumbers ns = ns >>= sequenceA . fmap rawEvent'

-- | Internal only. Always generates
-- a (non-offset) `FollowerMaze.EventQueue.emptyQueue`.
alignedNewQueue :: QC.Gen EventQueue
alignedNewQueue = pure emptyQueue

-- | Internal only. Generates an empty queue set to dequeue events up to some
-- small sequence number /larger than @1@/.
offsetNewQueue :: QC.Gen EventQueue
offsetNewQueue = newQueueAt <$> QC.choose (2, smallNum)

-- | Internal only. Generates a list of 0 or more __unique__,
-- unordered sequence numbers from @1@ up to some small sequence number.
withoutDuplicates :: QC.Gen [SequenceNumber]
withoutDuplicates = QC.sublistOf =<< QC.shuffle [1..smallNum]

-- | Internal only. Generates a list of 0 or more __possibly non-unique__,
-- unordered sequence numbers from @1@ up to some small sequence number.
withDuplicates :: QC.Gen [SequenceNumber]
withDuplicates = QC.listOf $ QC.choose (1, smallNum)

-- | Internal only. To test `EventQueue`s, it is helpful to keep the
-- maximum sequence number of enqeued events low so that the generated queue
-- will be more likely to contain immediately dequeueable events.
smallNum :: SequenceNumber
smallNum = 20
