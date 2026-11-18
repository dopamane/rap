module Data.Rap (Riff, getRiff, putRiff, Rap(..)) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Function
import Data.Word

type Riff = [Rap]

getRiff :: Get Riff
getRiff = many getRap

putRiff :: Riff -> Put
putRiff = foldMap putRap

data Rap = Stanza String String Riff | Verse String ByteString
  deriving (Eq, Read, Show)

getRap :: Get Rap
getRap = do
  (cid, csz) <- getHeader
  rap <- if cid == "RIFF" || cid == "LIST"
    then Stanza cid <$> fmap BSC.unpack (getLazyByteString 4) <*> getStanza (csz - 4)
    else Verse  cid <$> getLazyByteString (fromIntegral csz)
  getPad csz
  return rap

getStanza :: Word32 -> Get [Rap]
getStanza ssz = go 0
  where
    go cnt
      | cnt == ssz = return []
      | cnt >  ssz = fail "getStanza: overflow"
      | otherwise  = do
        (cid, csz) <- getHeader
        rap <- if cid == "RIFF" || cid == "LIST"
          then Stanza cid <$> fmap BSC.unpack (getLazyByteString 4) <*> getStanza (csz - 4)
          else Verse  cid <$> getLazyByteString (fromIntegral csz)
        getPad csz
        fmap (rap:) $ go $ applyWhen (odd csz) (+ 1) $ cnt + 8 + csz

getHeader :: Get (String, Word32)
getHeader = (,) <$> fmap BSC.unpack (getLazyByteString 4) <*> getWord32le

getPad :: Word32 -> Get ()
getPad csz = when (odd csz) (void getWord8)

putRap :: Rap -> Put
putRap r = case r of
  Stanza cid cty rs -> do
    putLazyByteString $ BSC.pack cid
    let (stz, csz) = putStanza rs
    putWord32le csz
    putLazyByteString $ BSC.pack cty
    stz
  Verse cid bs -> putVerse cid bs

putStanza :: Riff -> (Put, Word32)
putStanza = go 0
  where
    go cnt [] = (mempty, cnt)
    go cnt (r:rs) = case r of
      Verse cid bs ->
        let (p, cnt') = go (8 + verseSz bs + cnt) rs
        in (putVerse cid bs <> p, cnt')
      Stanza cid cty rs' -> undefined

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
