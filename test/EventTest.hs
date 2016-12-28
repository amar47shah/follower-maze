module EventTest (eventTests) where

import Config (numberOfUsers, totalEvents)
import Event (
    Event (Broadcast, Follow, Message, Unfollow, Update)
  , RawEvent
  , SequencedEvent (SequencedEvent)
  , parseRawEvent
  )

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.List (intercalate)

eventTests :: TestTree
eventTests = testGroup "Event" [parseRawEventTests]

parseRawEventTests:: TestTree
parseRawEventTests = testGroup "parseRawEvent" [parseRawEventProps]

parseRawEventProps :: TestTree
parseRawEventProps = testGroup "Properties"
  [ QC.testProperty "classifies follows"          propParseFollow
  , QC.testProperty "classifies unfollows"        propParseUnfollow
  , QC.testProperty "classifies broadcasts"       propParseBroadcast
  , QC.testProperty "classifies private messages" propParseMessage
  , QC.testProperty "classifies status updates"   propParseUpdate
  , QC.testProperty "parses sequence number"      propParseSequenceNum
  , QC.testProperty "parses user ids"             propParseUsers
  , QC.testProperty "preserves raw input"         propParsePreserves
  ]

propParseFollow :: QC.Property
propParseFollow =
  QC.forAll follow $ isFollow . parseRawEvent
      where
  isFollow (SequencedEvent _ Follow{}) = True
  isFollow _                           = False

propParseUnfollow :: QC.Property
propParseUnfollow =
  QC.forAll unfollow $ isUnfollow . parseRawEvent
      where
  isUnfollow (SequencedEvent _ Unfollow{}) = True
  isUnfollow _                             = False

propParseBroadcast :: QC.Property
propParseBroadcast =
  QC.forAll broadcast $ isBroadcast . parseRawEvent
      where
  isBroadcast (SequencedEvent _ Broadcast{}) = True
  isBroadcast _                              = False

propParseMessage :: QC.Property
propParseMessage =
  QC.forAll message $ isMessage . parseRawEvent
      where
  isMessage (SequencedEvent _ Message{}) = True
  isMessage _                            = False

propParseUpdate :: QC.Property
propParseUpdate =
  QC.forAll update $ isUpdate . parseRawEvent
      where
  isUpdate (SequencedEvent _ (Update _ _)) = True
  isUpdate _                               = False

propParseSequenceNum :: QC.Property
propParseSequenceNum =
  QC.forAll rawEvent $ correctSequenceNum <*> parseRawEvent
      where
  correctSequenceNum r (SequencedEvent s _) = show s == takeWhile (/= '|') r

propParseUsers :: QC.Property
propParseUsers =
  QC.forAll rawEvent $ correctUser <*> parseRawEvent
      where
  correctUser raw (SequencedEvent _ e) = correctUser' raw e
  correctUser' r (Follow    _ f t) = fmap show [f, t] == twoUserIds r
  correctUser' r (Unfollow  _ f t) = fmap show [f, t] == twoUserIds r
  correctUser' r (Message   _ f t) = fmap show [f, t] == twoUserIds r
  correctUser' r (Update    _ f  ) = show f == lastUserId r
  correctUser' _ _                 = True
  twoUserIds = sequenceA [secondToLastUserId, lastUserId]
  lastUserId = reverse . takeWhile (/= '|') . reverse
  secondToLastUserId =
    reverse . takeWhile (/= '|') . drop 1 . dropWhile (/= '|') . reverse

propParsePreserves :: QC.Property
propParsePreserves =
  QC.forAll rawEvent $ containsRaw <*> parseRawEvent
      where
  containsRaw raw (SequencedEvent _ e) = containsRaw' raw e
  containsRaw' r (Follow    r' _ _) = r == r'
  containsRaw' r (Unfollow  r' _ _) = r == r'
  containsRaw' r (Broadcast r'    ) = r == r'
  containsRaw' r (Message   r' _ _) = r == r'
  containsRaw' r (Update    r' _  ) = r == r'

rawEvent :: QC.Gen RawEvent
rawEvent = QC.oneof [follow, unfollow, broadcast, message, update]

follow, unfollow, broadcast, message, update :: QC.Gen RawEvent
follow    = rawEventGen "F" 2
unfollow  = rawEventGen "U" 2
broadcast = rawEventGen "B" 0
message   = rawEventGen "P" 2
update    = rawEventGen "S" 1

rawEventGen :: RawEvent -> Int -> QC.Gen RawEvent
rawEventGen c u =
  fmap (intercalate "|") . liftA2 (:) sequenceNum . fmap (c:) $ replicateM u user
      where
  sequenceNum = show <$> QC.choose (1, totalEvents)
  user        = show <$> QC.choose (1, numberOfUsers)
