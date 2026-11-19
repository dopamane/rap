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
  [ getEmptyTest
  , getVerseTest
  , putEmptyTest
  , putVerseTest
  ]

getEmptyTest :: TestTree
getEmptyTest = testCase "Get empty" $ runGet getRap mempty @?= []

getVerseTest :: TestTree
getVerseTest = testCase "Get verse" $ runGet getRap l33tVerseBS @?= l33tVerse

putEmptyTest :: TestTree
putEmptyTest = testCase "Put empty" $ runPut (putRap []) @?= mempty

putVerseTest :: TestTree
putVerseTest = testCase "Put verse" $ runPut (putRap l33tVerse) @?= l33tVerseBS

l33tVerse :: Rap
l33tVerse = [Verse "1337" $ BSC.pack "hello world"]

l33tVerseBS :: ByteString
l33tVerseBS = mconcat [BSC.pack "1337" <> runPut (putWord32le 11)
  , BSC.pack "hello world" <> BS.singleton 0x00]
