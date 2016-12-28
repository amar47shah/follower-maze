module Main where

import EventTest (eventTests)

import Test.Tasty
import Control.Monad (replicateM_)

main :: IO ()
main = replicateM_ 2 (putStrLn "") *> defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [eventTests]
