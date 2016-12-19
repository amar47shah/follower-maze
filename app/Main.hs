module Main where

import Control.Monad (replicateM_)
import Data.List (unfoldr)
import Network (PortID (PortNumber), accept, Socket, listenOn)
import System.IO (hClose, hGetLine)

main :: IO ()
main = do
  sockSource <- listen 9090
  (source, _, _) <- accept sockSource
  sockClient <- listen 9099
  replicateM_ 100 $ do
    (client, _, _) <- accept sockClient
    hClose client
  replicateM_ 10000000 $ hGetLine source >>= print . parseEvent
  hClose source

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

data Event = Event
  { eventRaw  :: String
  , eventSeq  :: Integer
  , eventComm :: Comm
  } deriving Show

data Comm = Message  User User
          | Follow   User User
          | Unfollow User User
          | Update   User
          | Broadcast
          deriving Show

type User = String

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
