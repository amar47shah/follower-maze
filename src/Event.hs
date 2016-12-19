module Event (Event (Event), Notification, UserId, Comm(..), parseEvent) where

import Data.List (unfoldr)

type Notification = String
type UserId       = String

data Event = Event
  { eventRaw  :: Notification
  , eventSeq  :: Integer
  , eventComm :: Comm
  }

data Comm = Message  UserId UserId
          | Follow   UserId UserId
          | Unfollow UserId UserId
          | Update   UserId
          | Broadcast

parseEvent :: String -> Event
parseEvent = match <*> splitOn '|'
      where
  match :: String -> [String] -> Event
  match r (n:"P":from:to:[]) = Event r (read n) $ Message  from to
  match r (n:"F":from:to:[]) = Event r (read n) $ Follow   from to
  match r (n:"U":from:to:[]) = Event r (read n) $ Unfollow from to
  match r (n:"S":from   :[]) = Event r (read n) $ Update   from
  match r (n:"B"        :[]) = Event r (read n) $ Broadcast
  match _ _                  = error "Unrecognized event"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = unfoldr . splitOnceOn
      where
  splitOnceOn :: Eq a => a -> [a] -> Maybe ([a], [a])
  splitOnceOn _ [] = Nothing
  splitOnceOn d xs = Just . (drop 1 <$>) $ break (== d) xs
