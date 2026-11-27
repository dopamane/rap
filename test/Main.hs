{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Rap
import Data.Word
import Paths_rap
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = getDataDir >>= \dataDir -> defaultMain $ testGroup "Data.Rap"
  [ getEmptyTest, getVerseTest, getStanzaTest
  , putEmptyTest, putVerseTest, putStanzaTest
  , tillICollapseTest, wavTest dataDir
  , putVerseTooLongTest, infiniteVerseTest, putStanzaTooLongTest, infStanzaTest
  , badVerseID, badStanzaForm
  ]

getEmptyTest :: TestTree
getEmptyTest = testCase "Get empty" $ runGet getRap mempty @?= []

getVerseTest :: TestTree
getVerseTest = testCase "Get verse" $ runGet getRap l33tVerseBS @?= l33tVerse

getStanzaTest :: TestTree
getStanzaTest = testCase "Get stanza" $
  runGet getRap sampStanzaBS @?= sampStanza

putEmptyTest :: TestTree
putEmptyTest = testCase "Put empty" $ fmap runPut (putRap []) @?= Right mempty

putVerseTest :: TestTree
putVerseTest = testCase "Put verse" $ fmap runPut (putRap l33tVerse) @?= Right l33tVerseBS

putStanzaTest :: TestTree
putStanzaTest = testCase "Put stanza" $
  fmap (BS.unpack . runPut) (putRap sampStanza) @?= Right (BS.unpack sampStanzaBS)

sampStanza :: Rap
sampStanza = [Stanza "RIFF" "riff" $ l33tVerse ++ krs1Verse]

sampStanzaBS :: ByteString
sampStanzaBS = mconcat ["RIFF"
  , runPut (putWord32le $ fromIntegral $ 4 + BS.length l33tVerseBS + BS.length krs1BS)
  , "riff" <> l33tVerseBS <> krs1BS]

l33tVerse :: Rap
l33tVerse = [Verse "1337" "hello world"]

l33tVerseBS :: ByteString
l33tVerseBS = mconcat ["1337" <> runPut (putWord32le 11)
  , BSC.pack "hello world" <> BS.singleton 0x00]

krs1Verse :: Rap
krs1Verse = [Verse "krs1" $ BSC.pack stepIntoAWorld]

krs1BS :: ByteString
krs1BS = mconcat [BSC.pack "krs1" <>
  runPut (putWord32le $ fromIntegral $ length stepIntoAWorld),
  BSC.pack stepIntoAWorld]

stepIntoAWorld :: String
stepIntoAWorld = "step into a world where theres no one left but the very best"

tillICollapse :: Rap
tillICollapse =
  [ Stanza "RIFF" "mnem"
  [ Verse "lin1" "Til I collapse, I'm spillin' these raps long as you feel em"
  , Verse "lin2" "Til the day that I drop, you'll never say that I'm not killin em"
  , Verse "lin3" "Cause when I am not, then Ima stop pennin em"
  , Verse "lin4" "And I am not hip-hop and Im just not Eminem"
  , Verse "lin5" "Subliminal throughts, when Ima stop sendin em?"
  , Verse "lin6" "Women are caught in webs, spin em and hock venom"
  , Verse "lin7" "Adrenaline shots of penicillin could not get the illin to stop"
  , Verse "lin8" "Amoxicillins just not real enough"
  , Verse "lin9" "The criminal, cop-killin, hip-hop villain"
  , Verse "li10" "A minimal swap to cop millions of Pac listeners"
  , Verse "li11" "Youre comin with me, feel it or not"
  , Verse "li12" "You're gonna fear it like I showed ya the spirit of God lives in us"
  , Verse "li13" "You hear it a lot, lyrics to shock"
  , Verse "li14" "Is it a miracle or am I just product of pop fizzin' up?"
  , Verse "li15" "Fa' shizzle, my wizzle, this is the plot, listen up"
  , Verse "li16" "You bizzles forgot, Slizzle does not give a fuck"
  ]]

tillICollapseTest :: TestTree
tillICollapseTest = testCase "TillICollapse" $
  fmap (runGet getRap . runPut) (putRap tillICollapse) @?= Right tillICollapse

wavTest :: FilePath -> TestTree
wavTest dataDir = testCase "wav" $ do
  bs <- BS.readFile $ dataDir </> "hello.wav"
  fmap runPut (putRap $ runGet getRap bs) @?= Right bs

putVerseTooLongTest :: TestTree
putVerseTooLongTest = testCase "verseTooLong" $
  (void . putRap) [Verse "fooo" $ BS.replicate (fromIntegral (maxBound :: Word32) + 1) 0xFF]
    @?= Left ("fooo byte length > " ++ show (maxBound :: Word32))

infiniteVerseTest :: TestTree
infiniteVerseTest = testCase "infiniteVerse" $
  (void . putRap) [Verse "fooo" $ BS.repeat 0xFF]
    @?= Left ("fooo byte length > " ++ show (maxBound :: Word32))

putStanzaTooLongTest :: TestTree
putStanzaTooLongTest = testCase "stanzaTooLong" $ do
  let stz = [Stanza "LIST" "fooo"
            [Verse "baar" $ BS.replicate (fromIntegral (maxBound :: Word32)) 0xFF]]
  (void . putRap) stz @?= Left ("LIST: fooo: length > " ++ show (maxBound :: Word32))

infStanzaTest :: TestTree
infStanzaTest = testCase "infStanza" $ do
  let stz = [Stanza "LIST" "fooo" $ repeat $ Verse "baar" $ BS.replicate 4096 0xFF]
  (void . putRap) stz @?= Left ("LIST: fooo: length > " ++ show (maxBound :: Word32))

badVerseID :: TestTree
badVerseID = testCase "badVerseID" $ do
  (void . putRap) [Verse "foo" mempty] @?= Left "expecting 4 byte ID: \"foo\""
  (void . putRap) [Verse "foobar" mempty] @?= Left "expecting 4 byte ID: \"foob...\""

badStanzaForm :: TestTree
badStanzaForm = testCaseSteps "badStanzaForm" $ \step -> do
  step "lvl 1"
  (void . putRap) [Stanza "foo" "baar" []] @?= Left "expecting 4 byte ID: \"foo\""
  (void . putRap) [Stanza "foobar" "baar" []] @?= Left "expecting 4 byte ID: \"foob...\""
  (void . putRap) [Stanza "fooo" "bar" []] @?= Left "expecting 4 byte type: \"bar\""
  (void . putRap) [Stanza "fooo" "barbaz" []] @?= Left "expecting 4 byte type: \"barb...\""
  step "lvl 2"
  (void . putRap) [Stanza "fooo" "baar" [Stanza "zoo" "cafe" []]]
    @?= Left "expecting 4 byte ID: \"zoo\""
  (void . putRap) [Stanza "fooo" "baar" [Stanza "zoozaa" "cafe" []]]
    @?= Left "expecting 4 byte ID: \"zooz...\""
  (void . putRap) [Stanza "fooo" "baar" [Stanza "zooo" "cafebeef" []]]
    @?= Left "expecting 4 byte type: \"cafe...\""
  (void . putRap) [Stanza "fooo" "baar" [Stanza "zooo" "caf" []]]
    @?= Left "expecting 4 byte type: \"caf\""
