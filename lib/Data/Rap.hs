module Data.Rap (Riff, getRiff, Rap(..)) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Function
import Data.Word

type Riff = [Rap]

getRiff :: Get Riff
getRiff = many getRap

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
{-
putRap :: [Rap] -> Put
putRap = undefined

instance Binary Rap where
  get = do
    cid <- BSC.unpack <$> getLazyByteString 4
    csz <- getWord32le
    if cid == "RIFF" || cid == "LIST"
      then do
        (fty, rps) <- isolate (fromIntegral csz) $ (,) <$> getLazyByteString 4 <*> many get
        pad csz
        return $ Stanza cid csz (BSC.unpack fty) rps
      else Verse cid <$> getLazyByteString (fromIntegral csz) <* pad csz
    where
      pad sz = when (odd sz) (void getWord8)

  put (Stanza cid csz fty rps) = do
    putLazyByteString $ BSC.pack cid
    putWord32le csz
    putLazyByteString $ BSC.pack fty
    foldMap put rps
  put (Verse cid cbs) = do
    putLazyByteString $ BSC.pack cid
    putWord32le csz
    putLazyByteString cbs
    when (odd csz) (putWord8 0x00)
    where
      csz = fromIntegral $ BS.length cbs
-}
