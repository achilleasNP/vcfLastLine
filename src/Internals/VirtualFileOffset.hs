module Internals.VirtualFileOffset
(VirtualFileOffset(..),
  virtualOffset)
where
import Data.Bits
import Data.Word
import Data.Int

data VirtualFileOffset = VirtualFileOffset {compressedFileOffset :: Integer, chunkOffset :: Int64} deriving (Show, Eq)

virtualOffset :: Word64 -> VirtualFileOffset
virtualOffset w = VirtualFileOffset fileOffset chunkOffset 
                  where
                      mask = shiftR (complement zeroBits) 48
                      fileOffset = fromIntegral $ shiftR w 16
                      chunkOffset = fromIntegral $ mask .&. w

