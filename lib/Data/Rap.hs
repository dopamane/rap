module Data.Rap (Rap(..), Four(..)) where

import Data.ByteString.Lazy (ByteString)
import Data.Word

data Rap = Stanza Four Four [Rap]
         | Verse  Four ByteString
  deriving (Eq, Read, Show)

data Four = Four{f3 :: Word8, f2 :: Word8, f1 :: Word8, f0 :: Word8}
  deriving (Eq, Read, Show)
{-
instance Binary Chunk where
  get = do
    cid <- BSC.unpack <$> getLazyByteString 4
    csz <- getWord32le
    Chunk cid <$> getLazyByteString (fromIntegral csz) <* when (odd csz) (void getWord8)

  put (Chunk cid cbs) = do
    putLazyByteString $ BSC.pack $ take 4 $ cid <> repeat ' '
    let csz = fromIntegral $ BS.length cbs
    putWord32le csz
    putLazyByteString $ BS.take (fromIntegral len) cbs
    when (odd csz) $ putWord8 0x00
-}
