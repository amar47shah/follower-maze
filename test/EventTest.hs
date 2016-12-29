module EventTest
  ( eventTests
  ) where

import Event (Event (..), RawEvent, SequencedEvent (..), eventRaw, parseRawEvent)
import Generators (broadcast, follow, message, rawEvent, unfollow, update)

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Data.Maybe (isNothing)

eventTests :: TestTree
eventTests = testGroup "Event" [parseRawEventTests]

parseRawEventTests:: TestTree
parseRawEventTests = testGroup "parseRawEvent"
  [ parseRawEventUnitTests
  , parseRawEventProps
  ]

parseRawEventUnitTests :: TestTree
parseRawEventUnitTests = testGroup "Unit Tests"
  [ HU.testCase "does not parse blank string"             $ noParse ""
  , HU.testCase "does not parse \"corrupted\""            $ noParse "corrupted"
  , HU.testCase "does not parse without delimiters"       $ noParse "43P3256"
  , HU.testCase "does not parse with wrong delimiters"    $ noParse "43 P,32/56"
  , HU.testCase "does not parse with < two fields"        $ noParse "12345"
  , HU.testCase "does not parse with > four fields"       $ noParse "3|P|2|6|5"
  , HU.testCase "does not parse with trailing text"       $ noParse "43|Bmore"
  , HU.testCase "does not parse without sequence number"  $ noParse "P|32|56"
  , HU.testCase "does not parse with unreadable number"   $ noParse "A3|P|32|56"
  , HU.testCase "does not parse with unreadable 1st user" $ noParse "43|P|B2|56"
  , HU.testCase "does not parse with unreadable 2nd user" $ noParse "43|P|32|f6"
  , HU.testCase "does not parse without a code"           $ noParse "43||32|56"
  , HU.testCase "does not parse with unrecognized code"   $ noParse "43|T|32|56"
  , HU.testCase "does not parse follow with one user"     $ noParse "666|F|60"
  , HU.testCase "does not parse unfollow with one user"   $ noParse "1|U|12|"
  , HU.testCase "does not parse message with one user"    $ noParse "43|P|32"
  , HU.testCase "does not parse update with no user"      $ noParse "634|S|"
  ]

noParse :: RawEvent -> HU.Assertion
noParse raw = isNothing (parseRawEvent raw) HU.@?= True

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
  QC.forAll follow $ maybe False isFollow . parseRawEvent
      where
  isFollow (SequencedEvent _ Follow{}) = True
  isFollow _                           = False

propParseUnfollow :: QC.Property
propParseUnfollow =
  QC.forAll unfollow $ maybe False isUnfollow . parseRawEvent
      where
  isUnfollow (SequencedEvent _ Unfollow{}) = True
  isUnfollow _                             = False

propParseBroadcast :: QC.Property
propParseBroadcast =
  QC.forAll broadcast $ maybe False isBroadcast . parseRawEvent
      where
  isBroadcast (SequencedEvent _ Broadcast{}) = True
  isBroadcast _                              = False

propParseMessage :: QC.Property
propParseMessage =
  QC.forAll message $ maybe False isMessage . parseRawEvent
      where
  isMessage (SequencedEvent _ Message{}) = True
  isMessage _                            = False

propParseUpdate :: QC.Property
propParseUpdate =
  QC.forAll update $ maybe False isUpdate . parseRawEvent
      where
  isUpdate (SequencedEvent _ (Update _ _)) = True
  isUpdate _                               = False

propParseSequenceNum :: QC.Property
propParseSequenceNum =
  QC.forAll rawEvent $ maybe True <$> correctSequenceNum <*> parseRawEvent
      where
  correctSequenceNum r (SequencedEvent s _) = show s == takeWhile (/= '|') r

propParseUsers :: QC.Property
propParseUsers =
  QC.forAll rawEvent $ maybe True <$> correctUser <*> parseRawEvent
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
  QC.forAll rawEvent $ maybe True <$> containsRaw <*> parseRawEvent
      where
  containsRaw raw (SequencedEvent _ e) = raw == eventRaw e
