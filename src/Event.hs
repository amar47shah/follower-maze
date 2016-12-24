module Event (Event (..), EventQueue, RawEvent, UserId, dequeueAll, emptyQueue, enqueueRaw) where

import Data.List (unfoldr)
import Data.Function (on)

type RawEvent = String
type UserId   = Integer

data Event = Message   RawEvent UserId UserId
           | Follow    RawEvent UserId UserId
           | Unfollow  RawEvent UserId UserId
           | Update    RawEvent UserId
           | Broadcast RawEvent

data SequencedEvent = SequencedEvent Integer Event

sequenceNumber :: SequencedEvent -> Integer
sequenceNumber (SequencedEvent s _) = s

instance Eq SequencedEvent where
  (==) = (==) `on` sequenceNumber

instance Ord SequencedEvent where
  compare = compare `on` sequenceNumber

parseRawEvent :: RawEvent -> SequencedEvent
parseRawEvent = match <*> splitOn '|'
      where
  match :: RawEvent -> [RawEvent] -> SequencedEvent
  match r [n, "P", from, to] = SequencedEvent (read n) $ Message   r (read from) (read to)
  match r [n, "F", from, to] = SequencedEvent (read n) $ Follow    r (read from) (read to)
  match r [n, "U", from, to] = SequencedEvent (read n) $ Unfollow  r (read from) (read to)
  match r [n, "S", from    ] = SequencedEvent (read n) $ Update    r (read from)
  match r [n, "B"          ] = SequencedEvent (read n) $ Broadcast r
  match _ _                  = error "Unrecognized event"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = unfoldr . splitOnceOn
      where
  splitOnceOn :: Eq a => a -> [a] -> Maybe ([a], [a])
  splitOnceOn _ [] = Nothing
  splitOnceOn d xs = Just . (drop 1 <$>) $ break (== d) xs

data EventQueue = EventQueue Integer (Heap SequencedEvent)

emptyQueue :: EventQueue
emptyQueue = EventQueue 1 Empty

enqueueRaw :: EventQueue -> RawEvent -> EventQueue
enqueueRaw q = enqueue q . parseRawEvent

dequeueAll :: EventQueue -> ([Event], EventQueue)
dequeueAll queue = (reverse backwards, flushed)
      where
  (backwards, flushed) = collectDequeued ([], queue)
  collectDequeued (es, q) =
    maybe (es, q) (\(e, r) -> collectDequeued (e:es, r)) $ dequeue q

enqueue :: EventQueue -> SequencedEvent -> EventQueue
enqueue (EventQueue n h) se = EventQueue n $ insert se h

dequeue :: EventQueue -> Maybe (Event, EventQueue)
dequeue (EventQueue n h) =
  findMin h >>= takeIfReady
      where
  takeIfReady (SequencedEvent s e) =
    if s > n
    then Nothing
    else Just (e, EventQueue (succ n) (deleteMin h))

-- Pairing Heap: https://en.wikipedia.org/wiki/Pairing_heap
data Heap a = Empty | Heap a [Heap a]

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
