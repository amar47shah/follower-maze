module EventTest
  ( eventTests
  ) where

import Event (Event (..), SequencedEvent (..), eventRaw, parseRawEvent)
import Generators (broadcast, follow, message, rawEvent, unfollow, update)

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

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
  containsRaw raw (SequencedEvent _ e) = raw == eventRaw e
