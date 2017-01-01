module Main where

import FollowerMaze.EventTest (eventTests)
import FollowerMaze.EventQueueTest (eventQueueTests)

import Test.Tasty
import Control.Monad (replicateM_)

main :: IO ()
main = replicateM_ 2 (putStrLn "") *> defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [eventTests, eventQueueTests]
