-- | Module
module EventQueue
  ( EventQueue (EventQueue)
  , dequeueAll
  , emptyQueue
  , enqueueRaw
  , newQueueAt
  ) where

import Event (Event, RawEvent, SequencedEvent (SequencedEvent), parseRawEvent)

-- | Exported
data EventQueue = EventQueue Integer (Heap SequencedEvent) deriving Show

-- | Exported
emptyQueue :: EventQueue
emptyQueue = newQueueAt 1

-- | Exported
newQueueAt :: Integer -> EventQueue
newQueueAt s = EventQueue s Empty

-- | Exported
enqueueRaw :: EventQueue -> RawEvent -> EventQueue
enqueueRaw q = maybe q (enqueue q) . parseRawEvent

enqueue :: EventQueue -> SequencedEvent -> EventQueue
enqueue (EventQueue n h) se = EventQueue n $ insert se h

-- | Exported
dequeueAll :: EventQueue -> ([Event], EventQueue)
dequeueAll queue = (reverse backwards, flushed)
      where
  (backwards, flushed) = collectDequeued ([], queue)
  collectDequeued :: ([Event], EventQueue) -> ([Event], EventQueue)
  collectDequeued (es, q) =
    maybe (es, q) (\(e, r) -> collectDequeued (e:es, r)) $ dequeue q

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

next :: EventQueue -> Maybe SequencedEvent
next (EventQueue _ h) = findMin h

-- | Internal
-- Pairing Heap: https://en.wikipedia.org/wiki/Pairing_heap
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
