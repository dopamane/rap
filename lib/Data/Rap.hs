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

putRap :: Rap -> Either String Put
putRap = fmap mconcat . mapM putRiff

data Riff = Verse String ByteString | Stanza String String Rap
  deriving (Eq, Read, Show)

getRiff :: Get Riff
getRiff = uncurry getElem =<< getHeader

getElems :: Word32 -> Get Rap
getElems ssz = go 0
  where
    go cnt
      | cnt == ssz = return []
      | cnt >  ssz = fail "getElems: overflow"
      | otherwise  = do
        (cid, csz) <- getHeader
        (:) <$> getElem cid csz <*> go (applyWhen (odd csz) (+ 1) $ cnt + 8 + csz)

getElem :: String -> Word32 -> Get Riff
getElem cid csz
  | cid == "RIFF" || cid == "LIST" =
    Stanza cid <$> fmap BSC.unpack (getLazyByteString 4) <*> getElems (csz - 4)
  | otherwise = Verse cid <$> getLazyByteString (fromIntegral csz) <* getPad csz

getHeader :: Get (String, Word32)
getHeader = (,) <$> fmap BSC.unpack (getLazyByteString 4) <*> getWord32le

getPad :: Word32 -> Get ()
getPad csz = when (odd csz) (void getWord8)

putRiff :: Riff -> Either String Put
putRiff r = case r of
  Verse cid bs -> putVerse cid bs
  Stanza cid cty rs -> case putElems rs of
    Left err -> Left err
    Right (es, esz)
      | esz + 4 > fromIntegral (maxBound :: Word32) ->
        Left $ cid ++ ": " ++ cty ++ ": length too long: " ++ show (esz + 4)
      | otherwise -> Right $ do
        putLazyByteString $ BSC.pack cid
        putWord32le $ fromIntegral $ esz + 4
        putLazyByteString $ BSC.pack cty
        es

putElems :: Rap -> Either String (Put, Integer)
putElems = accumElems . map putElem

accumElems :: [Either String (Put, Integer)] -> Either String (Put, Integer)
accumElems ees = do
  es <- sequence ees
  let (ps, cs) = unzip es
  return (mconcat ps, sum cs)

putElem :: Riff -> Either String (Put, Integer)
putElem r = case r of
  Verse cid bs -> do
    let cnt = fromIntegral $ BS.length bs
    v <- putVerse cid bs
    return (v, 8 + applyWhen (odd cnt) (+ 1) cnt)
  Stanza cid cty rs -> case putElems rs of
    Left err -> Left err
    Right (es, cnt)
      | 4 + cnt > fromIntegral (maxBound :: Word32) ->
        Left $ cid ++ ": " ++ cty ++ ": length too long: " ++ show (4 + cnt)
      | otherwise ->
        let p = mconcat [putLazyByteString $ BSC.pack cid, putWord32le $ fromIntegral (4 + cnt)
                , putLazyByteString $ BSC.pack cty, es]
        in Right (p, 8 + 4 + cnt)

putVerse :: String -> ByteString -> Either String Put
putVerse cid bs
  | l > fromIntegral (maxBound :: Word32) = Left $ cid ++ " bytes too long: " ++ show l
  | otherwise = Right $ do
    putLazyByteString $ BSC.pack cid
    let csz = fromIntegral l
    putWord32le csz
    putLazyByteString bs
    when (odd csz) $ putWord8 0x00
  where
    l = BS.length bs
