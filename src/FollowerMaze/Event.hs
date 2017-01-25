-- | `Event` and the associated types `RawEvent`, `SequencedEvent`,
-- `SequenceNumber`, and `UserId` represent raw and parsed event data.
module FollowerMaze.Event
  ( Event (Message, Follow, Unfollow, Update, Broadcast)
  , UserId
  , RawEvent
  , eventRaw
  , SequencedEvent (SequencedEvent)
  , SequenceNumber
  , parseRawEvent
  ) where

import Data.Function (on)
import Data.List (unfoldr)
import Safe (readMay)

-- | There are five possible events.
-- All events contain their original, raw representation.
--
-- `Message`, `Follow`, and `Unfollow` events each contain two user identifiers.
--
-- `Update` events contain one user identifier.
--
-- `Broadcast` events contain no user identifiers.
data Event = Message   RawEvent UserId UserId  -- ^ A private message from one user to another.
           | Follow    RawEvent UserId UserId  -- ^ A subscription by one user to another's status updates.
           | Unfollow  RawEvent UserId UserId  -- ^ The termination of a subscription.
           | Update    RawEvent UserId         -- ^ A status update from a user to their followers.
           | Broadcast RawEvent                -- ^ A special update served to all users.
           deriving Show

-- | An identifier representing a unique user.
type UserId   = Integer

-- | All or part of an unparsed event.
type RawEvent = String

-- | Returns the unparsed representation of a parsed event.
eventRaw :: Event -> RawEvent
eventRaw (Message   r _ _) = r
eventRaw (Follow    r _ _) = r
eventRaw (Unfollow  r _ _) = r
eventRaw (Update    r _  ) = r
eventRaw (Broadcast r    ) = r

-- | Every unparsed event contains a sequence number indicating its
-- ordered placement.
--
-- `SequencedEvent` collects a parsed event and its sequence number.
data SequencedEvent = SequencedEvent SequenceNumber Event deriving Show

-- | A number indicating an event's placement in the ordering of all events.
type SequenceNumber = Integer

sequenceNumber :: SequencedEvent -> SequenceNumber
sequenceNumber (SequencedEvent s _) = s

instance Eq SequencedEvent where
  (==) = (==) `on` sequenceNumber

instance Ord SequencedEvent where
  compare = compare `on` sequenceNumber

-- | Parse a raw event into structured data containing its sequence number,
-- event type, and all required user identifiers.
--
-- `parseRawEvent` returns `Nothing` in the following cases:
--
--   * the sequence number is not present
--   * fields are not delimited by the pipe character (@|@)
--   * a leading or trailing delimiter is present
--   * the event code is not @P@, @F@, @U@, @S@, or @B@
--   * the sequence number or user identifiers cannot be read
--   * too few or too many fields are present for the encoded event
--
-- The table below illustrates the parsing of raw events.
--
-- @
-- | raw           | parseRawEvent raw                                            |
-- |---------------|--------------------------------------------------------------|
-- | "43|P|32|56"  | Just (SequencedEvent     43 (Message   "43|P|32|56"  32 56)) |
-- | "666|F|60|50" | Just (SequencedEvent    666 (Follow    "666|F|60|50" 60 50)) |
-- | "1|U|12|9"    | Just (SequencedEvent      1 (Unfollow  "1|U|12|9"    12  9)) |
-- | "634|S|32"    | Just (SequencedEvent    634 (Update    "634|S|32"    32   )) |
-- | "542532|B"    | Just (SequencedEvent 542532 (Broadcast "542532|B"         )) |
-- | "P|32|56"     | Nothing                                                      |
-- | "43,P,32,56"  | Nothing                                                      |
-- | "|43|P|32|56" | Nothing                                                      |
-- | "43|P|32|56|" | Nothing                                                      |
-- | "43|p|32|56"  | Nothing                                                      |
-- | "A3|P|32|56"  | Nothing                                                      |
-- | "43|P|32"     | Nothing                                                      |
-- @
parseRawEvent :: RawEvent -> Maybe SequencedEvent
parseRawEvent = match <*> splitOn '|' . (++ "|")
      where
  -- | Attempt to match fields to a recognized event structure.
  match :: RawEvent -> [RawEvent] -> Maybe SequencedEvent
  match r [n, "P", f, t] = maybeSE n $ Message   r <$> readMay f <*> readMay t
  match r [n, "F", f, t] = maybeSE n $ Follow    r <$> readMay f <*> readMay t
  match r [n, "U", f, t] = maybeSE n $ Unfollow  r <$> readMay f <*> readMay t
  match r [n, "S", f   ] = maybeSE n $ Update    r <$> readMay f
  match r [n, "B"      ] = maybeSE n . Just $ Broadcast r
  match _ _              = Nothing
  -- | Attempt to read the sequence number and produce a SequencedEvent.
  maybeSE :: RawEvent -> Maybe Event -> Maybe SequencedEvent
  maybeSE numField me = SequencedEvent <$> readMay numField <*> me

-- | Internal only. Split a list into sublists on occurences of
-- a delimiter value, dropping the delimiter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = unfoldr . splitOnceOn
      where
  splitOnceOn :: Eq a => a -> [a] -> Maybe ([a], [a])
  splitOnceOn _ [] = Nothing
  splitOnceOn d xs = Just . (drop 1 <$>) $ break (== d) xs
