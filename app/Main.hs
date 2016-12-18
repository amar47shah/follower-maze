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

data Event = Message  User User
           | Follow   User User
           | Unfollow User User
           | Update   User
           | Broadcast
           deriving Show

type User = String

parseEvent :: String -> (Integer, Event)
parseEvent = match . splitOn '|'
      where
  match :: [String] -> (Integer, Event)
  match (n:"P":from:to:[]) = (read n, Message  from to)
  match (n:"F":from:to:[]) = (read n, Follow   from to)
  match (n:"U":from:to:[]) = (read n, Unfollow from to)
  match (n:"S":from   :[]) = (read n, Update   from)
  match (n:"B"        :[]) = (read n, Broadcast)
  match _                  = error "Unrecognized event"

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = unfoldr . splitOnceOn
      where
  splitOnceOn :: Eq a => a -> [a] -> Maybe ([a], [a])
  splitOnceOn _ [] = Nothing
  splitOnceOn d xs = Just . (drop 1 <$>) $ break (== d) xs
