-- | Module
module EventQueue
  ( EventQueue (EventQueue)
  , dequeueAll
  , emptyQueue
  , enqueueRaw
  ) where

import Event (Event, RawEvent, SequencedEvent (SequencedEvent), parseRawEvent)

-- | Exported
data EventQueue = EventQueue Integer (Heap SequencedEvent) deriving Show

-- | Exported
emptyQueue :: EventQueue
emptyQueue = EventQueue 1 Empty

-- | Exported
enqueueRaw :: EventQueue -> RawEvent -> EventQueue
enqueueRaw q = enqueue q . parseRawEvent

enqueue :: EventQueue -> SequencedEvent -> EventQueue
enqueue (EventQueue n h) se = EventQueue n $ insert se h

-- | Exported
dequeueAll :: EventQueue -> ([Event], EventQueue)
dequeueAll queue = (reverse backwards, flushed)
      where
  (backwards, flushed) = collectDequeued ([], queue)
  collectDequeued (es, q) =
    maybe (es, q) (\(e, r) -> collectDequeued (e:es, r)) $ dequeue q

dequeue :: EventQueue -> Maybe (Event, EventQueue)
dequeue (EventQueue n h) =
  findMin h >>= takeIfReady
      where
  takeIfReady (SequencedEvent s e) =
    if s > n
    then Nothing
    else Just (e, EventQueue (succ n) (deleteMin h))

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
