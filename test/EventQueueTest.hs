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
  [ QC.testProperty "dequeues events in successive sequence" propDequeueAllSucc
  , QC.testProperty "queue expects next sequence number"     propDequeueAllNextSucc
  , QC.testProperty "next number same after empty dequeue"   propDequeueAllNextSame
  , QC.testProperty "second dequeue in a row is empty"       propDequeueAllTwiceEmpty
  ]

propDequeueAllSucc :: QC.Property
propDequeueAllSucc =
  QC.forAll eventQueue $ isSuccSequence . fst . dequeueAll
      where
  isSuccSequence :: [Event] -> Bool
  isSuccSequence = isSucc . map sequenceNum
  isSucc :: (Enum a, Eq a) => [a] -> Bool
  isSucc = and . (zipWith ((==) . succ) <*> tail)

propDequeueAllNextSucc :: QC.Property
propDequeueAllNextSucc =
  QC.forAll eventQueue $ queueExpectsNext . dequeueAll
      where
  queueExpectsNext :: ([Event], EventQueue) -> Bool
  queueExpectsNext ([], _             ) = True
  queueExpectsNext (es, EventQueue n _) = sequenceNum (last es) == pred n

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
