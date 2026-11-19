module Main (main) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Rap
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Data.Rap"
  [ getEmptyTest
  , getVerseTest
  ]

getEmptyTest :: TestTree
getEmptyTest = testCase "Get empty" $ runGet getRap mempty @?= []

getVerseTest :: TestTree
getVerseTest = testCase "Get verse" $
  runGet getRap bs @?= [Verse "1337" $ BSC.pack "hello world"]
  where
    bs = mconcat [BSC.pack "1337" <> runPut (putWord32le 11)
      , BSC.pack "hello world" <> BS.singleton 0x00]
