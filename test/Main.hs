{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Rap
import Paths_rap
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = getDataDir >>= \dataDir -> defaultMain $ testGroup "Data.Rap"
  [ getEmptyTest, getVerseTest, getStanzaTest
  , putEmptyTest, putVerseTest, putStanzaTest
  , tillICollapseTest, wavTest dataDir
  ]

getEmptyTest :: TestTree
getEmptyTest = testCase "Get empty" $ runGet getRap mempty @?= []

getVerseTest :: TestTree
getVerseTest = testCase "Get verse" $ runGet getRap l33tVerseBS @?= l33tVerse

getStanzaTest :: TestTree
getStanzaTest = testCase "Get stanza" $
  runGet getRap sampStanzaBS @?= sampStanza

putEmptyTest :: TestTree
putEmptyTest = testCase "Put empty" $ runPut (putRap []) @?= mempty

putVerseTest :: TestTree
putVerseTest = testCase "Put verse" $ runPut (putRap l33tVerse) @?= l33tVerseBS

putStanzaTest :: TestTree
putStanzaTest = testCase "Put stanza" $
  BS.unpack (runPut (putRap sampStanza)) @?= BS.unpack sampStanzaBS

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
  runGet getRap (runPut $ putRap tillICollapse) @?= tillICollapse

wavTest :: FilePath -> TestTree
wavTest dataDir = testCase "wav" $ do
  bs <- BS.readFile $ dataDir </> "hello.wav"
  runPut (putRap $ runGet getRap bs) @?= bs
