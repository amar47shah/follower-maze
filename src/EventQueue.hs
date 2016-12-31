-- | `EventQueue`,
-- a specialized priority queue for events, supporting two operations:
--
-- * `enqueueRaw`: parse a raw event and insert its structured data into the queue.
-- * `dequeueAll`: remove and return all ready events in ascending sequence.
module EventQueue
  ( EventQueue (EventQueue)
  , emptyQueue
  , newQueueAt
  , enqueueRaw
  , dequeueAll
  , Heap
  ) where

import Event
  ( Event
  , RawEvent
  , SequencedEvent (SequencedEvent)
  , SequenceNumber
  , parseRawEvent
  )

-- | An `EventQueue` contains `SequencedEvent`s arranged in a /min heap/
-- as well as the sequence number of the /largest currently dequeueable event/,
-- which may or may not be currently contained in the queue.
data EventQueue = EventQueue SequenceNumber (Heap SequencedEvent) deriving Show

-- | An empty queue whose first dequeued event will have a sequence number
-- less than or equal to @1@.
emptyQueue :: EventQueue
emptyQueue = newQueueAt 1

-- | Create an empty queue that is ready to dequeue events up to the given
-- sequence number.
--
-- Used for testing the behavior of (unexpectedly) offset `EventQueue`s.
newQueueAt :: SequenceNumber -> EventQueue
newQueueAt s = EventQueue s Empty

-- | Attempt to parse and insert the given raw event. If the raw event cannot
-- be parsed, return the original queue.
enqueueRaw :: EventQueue -> RawEvent -> EventQueue
enqueueRaw q = maybe q (enqueue q) . parseRawEvent

-- | Internal only. Insert a parsed `SequencedEvent` into an `EventQueue`.
enqueue :: EventQueue -> SequencedEvent -> EventQueue
enqueue (EventQueue n h) se = EventQueue n $ insert se h

-- | Return all dequeueable events in ascending sequence order /and/
-- the resulting queue.
--
-- The returned events are removed from the accompanying queue, and the queue's
-- next expected sequence number is adjusted upward as necessary to exceed the
-- dequeued events' sequence numbers.
--
-- If no contained events have a low enough sequence number to be dequeued,
-- `dequeueAll` returns an empty event list and the unchanged queue.
dequeueAll :: EventQueue -> ([Event], EventQueue)
dequeueAll queue = (reverse backwards, flushed)
      where
  (backwards, flushed) = collectDequeuedBackwards ([], queue)
  -- | Recursively dequeue, returning a list of all ready events and the
  -- resulting queue. For efficiency, each newly dequeued event is attached to
  -- the /beginning/ of the list, requiring a final reversal.
  collectDequeuedBackwards :: ([Event], EventQueue) -> ([Event], EventQueue)
  collectDequeuedBackwards (es, q) =
    maybe (es, q) (\(e, r) -> collectDequeuedBackwards (e:es, r)) $ dequeue q

-- | Internal only. Remove and return the minimum sequence event /only if/
-- its sequence number is (less than or) equal to the current value of
-- the next expected sequence number, incrementing the expected sequence
-- number once a matching event has been dequeued.
--
-- Returns `Nothing` if no event can be dequeued.
dequeue :: EventQueue -> Maybe (Event, EventQueue)
dequeue queue =
  next queue >>= takeIfReady queue
      where
  takeIfReady :: EventQueue -> SequencedEvent -> Maybe (Event, EventQueue)
  takeIfReady (EventQueue n h) (SequencedEvent s e) =
    case compare n s of
      GT -> Just (e, EventQueue       n  $ deleteMin h)
      EQ -> Just (e, EventQueue (succ n) $ deleteMin h)
      _  -> Nothing

-- | Internal only. Return the smallest `SequencedEvent` in the queue,
-- without dequeueing.
next :: EventQueue -> Maybe SequencedEvent
next (EventQueue _ h) = findMin h

-- | Implementation of a pairing heap.
--
-- from Wikipedia: <https://en.wikipedia.org/wiki/Pairing_heap>
--
-- A pairing heap is a type of heap data structure with relatively simple
-- implementation and excellent practical amortized performance, introduced
-- by Michael Fredman, Robert Sedgewick, Daniel Sleator, and Robert Tarjan
-- in 1986.
--
-- A min-heap supports the following operations:
--
-- * /find-min/: simply return the root element of the heap.
-- * /merge/: compare the two root elements, the smaller remains the root of
--   the result, the larger element and its subtree is appended as a child of
--   this root.
-- * /insert/: create a new heap for the inserted element and merge into the
--   original heap.
-- * /delete-min/: remove the root and merge its subheaps. This implementation
--   uses a recursive "pairing" strategy, merging heaps in pairs first from
--   left-to-right and finally from right-to-left.
--
-- The amortized time per /delete-min/ is O(log n), and the operations
-- /find-min/, /merge/, and /insert/ run in O(1) amortized time.
data Heap a = Empty | Heap a [Heap a] deriving Show

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap x [])

findMin :: Heap a -> Maybe a
findMin (Heap h _) = Just h
findMin _          = Nothing

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap _ hs) = mergePairs hs
deleteMin Empty       = Empty

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []       = Empty
mergePairs [h]      = h
mergePairs (h:k:hs) = merge (merge h k) $ mergePairs hs

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h@(Heap x hs) k@(Heap y ks)
  | x < y     = Heap x (k:hs)
  | otherwise = Heap y (h:ks)
