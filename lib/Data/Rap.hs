module Data.Rap (Rap, getRap, putRap, Riff(..)) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Function
import Data.Word

type Rap = [Riff]

getRap :: Get Rap
getRap = many getRiff

putRap :: Rap -> Put
putRap = foldMap putRiff

data Riff = Verse String ByteString | Stanza String String Rap
  deriving (Eq, Read, Show)

getRiff :: Get Riff
getRiff = do
  (cid, csz) <- getHeader
  r <- if cid == "RIFF" || cid == "LIST"
    then Stanza cid <$> fmap BSC.unpack (getLazyByteString 4) <*> getElems (csz - 4)
    else Verse  cid <$> getLazyByteString (fromIntegral csz)
  getPad csz
  return r

getElems :: Word32 -> Get Rap
getElems ssz = go 0
  where
    go cnt
      | cnt == ssz = return []
      | cnt >  ssz = fail "getElems: overflow"
      | otherwise  = do
        (cid, csz) <- getHeader
        e <- if cid == "RIFF" || cid == "LIST"
          then Stanza cid <$> fmap BSC.unpack (getLazyByteString 4) <*> getElems (csz - 4)
          else Verse  cid <$> getLazyByteString (fromIntegral csz)
        getPad csz
        fmap (e:) $ go $ applyWhen (odd csz) (+ 1) $ cnt + 8 + csz

getHeader :: Get (String, Word32)
getHeader = (,) <$> fmap BSC.unpack (getLazyByteString 4) <*> getWord32le

getPad :: Word32 -> Get ()
getPad csz = when (odd csz) (void getWord8)

putRiff :: Riff -> Put
putRiff r = case r of
  Verse cid bs -> putVerse cid bs
  Stanza cid cty rs -> do
    putLazyByteString $ BSC.pack cid
    let (es, esz) = putElems rs
    putWord32le $ esz + 4
    putLazyByteString $ BSC.pack cty
    es

putElems :: Rap -> (Put, Word32)
putElems = accumElems . map putElem

accumElems :: [(Put, Word32)] -> (Put, Word32)
accumElems es = let (ps, cs) = unzip es
                in (mconcat ps, sum cs)

putElem :: Riff -> (Put, Word32)
putElem r = case r of
  Verse cid bs -> (putVerse cid bs, 8 + verseSz bs)
  Stanza cid cty rs ->
    let (es, cnt) = putElems rs
        p = mconcat [putLazyByteString $ BSC.pack cid, putWord32le $ 4 + cnt
          , putLazyByteString $ BSC.pack cty, es]
    in (p, 8 + 4 + cnt)

putVerse :: String -> ByteString -> Put
putVerse cid bs = do
  putLazyByteString $ BSC.pack cid
  let csz = verseSz bs
  putWord32le csz
  putLazyByteString bs
  when (odd csz) $ putWord8 0x00

verseSz :: ByteString -> Word32
verseSz bs = fromIntegral $ applyWhen (odd csz) (+ 1) csz
  where
    csz = BS.length bs
