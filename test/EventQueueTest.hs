module EventQueueTest
  ( eventQueueTests
  ) where

import Event (Event, eventRaw)
import EventQueue (EventQueue (EventQueue), dequeueAll)
import Generators (eventQueue)

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

eventQueueTests :: TestTree
eventQueueTests = testGroup "EventQueue" [dequeueAllTests]

dequeueAllTests:: TestTree
dequeueAllTests = testGroup "dequeueAll" [dequeueAllProps]

dequeueAllProps :: TestTree
dequeueAllProps = testGroup "Properties"
  [ QC.testProperty "dequeues events in ascending sequence" propDequeueAllNonDec
  , QC.testProperty "next number larger than dequeued"      propDequeueAllNextLarger
  , QC.testProperty "next number same after empty dequeue"  propDequeueAllNextSame
  , QC.testProperty "second dequeue in a row is empty"      propDequeueAllTwiceEmpty
  ]

propDequeueAllNonDec :: QC.Property
propDequeueAllNonDec =
  QC.forAll eventQueue $ isNonDecSequence . fst . dequeueAll
      where
  isNonDecSequence :: [Event] -> Bool
  isNonDecSequence = isNonDec . map sequenceNum
  isNonDec :: Ord a => [a] -> Bool
  isNonDec = and . (zipWith (<=) <*> tail)

propDequeueAllNextLarger :: QC.Property
propDequeueAllNextLarger =
  QC.forAll eventQueue $ nextExpectedLarger . dequeueAll
      where
  nextExpectedLarger :: ([Event], EventQueue) -> Bool
  nextExpectedLarger ([], _             ) = True
  nextExpectedLarger (es, EventQueue n _) = sequenceNum (last es) < n

propDequeueAllNextSame :: QC.Property
propDequeueAllNextSame =
  QC.forAll eventQueue $
    \q@(EventQueue oldN _) -> queueNextPreserved oldN $ dequeueAll q
      where
  queueNextPreserved :: Integer -> ([Event], EventQueue) -> Bool
  queueNextPreserved n ([], EventQueue n' _) = n == n'
  queueNextPreserved _ _                     = True

propDequeueAllTwiceEmpty :: QC.Property
propDequeueAllTwiceEmpty =
  QC.forAll eventQueue $ null . fst . dequeueAll . snd . dequeueAll

sequenceNum :: Event -> Integer
sequenceNum = read . takeWhile (/= '|') . eventRaw
