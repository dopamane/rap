module Main (main) where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Rap
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Data.Rap"
  [ getEmptyTest, getVerseTest, getVersesTest
  , putEmptyTest, putVerseTest
  ]

getEmptyTest :: TestTree
getEmptyTest = testCase "Get empty" $ runGet getRap mempty @?= []

getVerseTest :: TestTree
getVerseTest = testCase "Get verse" $ runGet getRap l33tVerseBS @?= l33tVerse

getVersesTest :: TestTree
getVersesTest = testCase "Get verses" $ runGet getRap (l33tVerseBS <> krs1BS)
  @?= l33tVerse ++ krs1Verse

putEmptyTest :: TestTree
putEmptyTest = testCase "Put empty" $ runPut (putRap []) @?= mempty

putVerseTest :: TestTree
putVerseTest = testCase "Put verse" $ runPut (putRap l33tVerse) @?= l33tVerseBS

l33tVerse :: Rap
l33tVerse = [Verse "1337" $ BSC.pack "hello world"]

l33tVerseBS :: ByteString
l33tVerseBS = mconcat [BSC.pack "1337" <> runPut (putWord32le 11)
  , BSC.pack "hello world" <> BS.singleton 0x00]

krs1Verse :: Rap
krs1Verse = [Verse "krs1" $ BSC.pack stepIntoAWorld]

krs1BS :: ByteString
krs1BS = mconcat [BSC.pack "krs1" <>
  runPut (putWord32le $ fromIntegral $ length stepIntoAWorld),
  BSC.pack stepIntoAWorld]

stepIntoAWorld :: String
stepIntoAWorld = "step into a world where theres no one left but the very best"
