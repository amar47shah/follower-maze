-- | Module
module Event (
    Event (Broadcast, Follow, Message, Unfollow, Update)
  , RawEvent
  , SequencedEvent (SequencedEvent)
  , UserId
  , parseRawEvent
  ) where

import Data.List (unfoldr)
import Data.Function (on)

-- | Exported
data Event = Message   RawEvent UserId UserId  -- ^ Stub
           | Follow    RawEvent UserId UserId  -- ^ Stub
           | Unfollow  RawEvent UserId UserId  -- ^ Stub
           | Update    RawEvent UserId         -- ^ Stub
           | Broadcast RawEvent                -- ^ Stub

-- | Exported
type RawEvent = String   -- ^ Stub
type UserId   = Integer  -- ^ Stub

-- | Exported
data SequencedEvent = SequencedEvent Integer Event

sequenceNumber :: SequencedEvent -> Integer
sequenceNumber (SequencedEvent s _) = s

instance Eq SequencedEvent where
  (==) = (==) `on` sequenceNumber

instance Ord SequencedEvent where
  compare = compare `on` sequenceNumber

-- | Exported
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
