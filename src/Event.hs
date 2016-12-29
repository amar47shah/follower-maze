-- | Module
module Event
  ( Event (Broadcast, Follow, Message, Unfollow, Update)
  , RawEvent
  , SequencedEvent (SequencedEvent)
  , UserId
  , eventRaw
  , parseRawEvent
  ) where

import Data.List (unfoldr)
import Data.Function (on)
import Safe (readMay)

-- | Exported
data Event = Message   RawEvent UserId UserId  -- ^ Stub
           | Follow    RawEvent UserId UserId  -- ^ Stub
           | Unfollow  RawEvent UserId UserId  -- ^ Stub
           | Update    RawEvent UserId         -- ^ Stub
           | Broadcast RawEvent                -- ^ Stub
           deriving Show

-- | Exported
type RawEvent = String
-- | Exported
type UserId   = Integer

-- | Exported
eventRaw :: Event -> RawEvent
eventRaw (Follow    r _ _) = r
eventRaw (Unfollow  r _ _) = r
eventRaw (Broadcast r    ) = r
eventRaw (Message   r _ _) = r
eventRaw (Update    r _  ) = r

-- | Exported
data SequencedEvent = SequencedEvent Integer Event deriving Show

sequenceNumber :: SequencedEvent -> Integer
sequenceNumber (SequencedEvent s _) = s

instance Eq SequencedEvent where
  (==) = (==) `on` sequenceNumber

instance Ord SequencedEvent where
  compare = compare `on` sequenceNumber

-- | Exported
parseRawEvent :: RawEvent -> Maybe SequencedEvent
parseRawEvent = match <*> splitOn '|'
      where
  match :: RawEvent -> [RawEvent] -> Maybe SequencedEvent
  match r [n, "P", f, t] = maybeSE n $ Message   r <$> readMay f <*> readMay t
  match r [n, "F", f, t] = maybeSE n $ Follow    r <$> readMay f <*> readMay t
  match r [n, "U", f, t] = maybeSE n $ Unfollow  r <$> readMay f <*> readMay t
  match r [n, "S", f   ] = maybeSE n $ Update    r <$> readMay f
  match r [n, "B"      ] = maybeSE n . Just $ Broadcast r
  match _ _              = Nothing
  maybeSE :: RawEvent -> Maybe Event -> Maybe SequencedEvent
  maybeSE numField me = SequencedEvent <$> readMay numField <*> me

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = unfoldr . splitOnceOn
      where
  splitOnceOn :: Eq a => a -> [a] -> Maybe ([a], [a])
  splitOnceOn _ [] = Nothing
  splitOnceOn d xs = Just . (drop 1 <$>) $ break (== d) xs
