module Event (Event (..), EventQueue, RawEvent, UserId, emptyQueue, nextEvent) where

import Data.List (unfoldr)

type RawEvent = String
type UserId   = Integer

data SequencedEvent = SequencedEvent Integer Event

data Event = Message   RawEvent UserId UserId
           | Follow    RawEvent UserId UserId
           | Unfollow  RawEvent UserId UserId
           | Update    RawEvent UserId
           | Broadcast RawEvent

data EventQueue = EventQueue [SequencedEvent]

emptyQueue :: EventQueue
emptyQueue = EventQueue []

enqueue :: EventQueue -> SequencedEvent -> EventQueue
enqueue (EventQueue ss) s = EventQueue (s:ss)

dequeue :: EventQueue -> (Maybe Event, EventQueue)
dequeue (EventQueue ((SequencedEvent _ e):ss)) = (Just e, EventQueue ss)
dequeue _                                      = (Nothing, emptyQueue)

nextEvent :: EventQueue -> RawEvent -> (Maybe Event, EventQueue)
nextEvent q = dequeue . enqueue q . parseRawEvent

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
