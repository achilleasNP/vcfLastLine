module Internals.DecompressChunk
(
   smartBlockDecompress
)
where
import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO
import Control.Exception

{-- | A custom decompressor so that we decompress only one chunk at a time
  and not the whole file. The decompress uses the option Zlib.decompressAllMembers   
  to force the decompression only of the current chunk.
 --}
myParams = Zlib.defaultDecompressParams {Zlib.decompressAllMembers = False}


chunkDecompress ::  Handle  -> Integer -> IO BSL.ByteString
chunkDecompress h pos = do
                              hSeek h AbsoluteSeek pos
                              let decompressor = Zlib.decompressIO Zlib.gzipFormat myParams
                              BSL.fromChunks <$> auxDecompress decompressor h 


auxDecompress :: Zlib.DecompressStream IO -> Handle -> IO [BS.ByteString]
auxDecompress (Zlib.DecompressInputRequired f) h = do
                                                  s <- BS.hGet h 4096
                                                  res <- f s
                                                  auxDecompress res h

auxDecompress (Zlib.DecompressOutputAvailable b next) h = do
                                                         res <- next
                                                         bs <- auxDecompress res h
                                                         return (b:bs)

                                                         
auxDecompress (Zlib.DecompressStreamEnd _) _ = return []

auxDecompress (Zlib.DecompressStreamError e) _ = throw e



-- | Block decompressor decompresses chunk from start to end position 
blockDecompress ::  Handle -> Integer -> Integer -> IO BSL.ByteString
blockDecompress h start end = do
                                 hSeek h AbsoluteSeek start
                                 GZ.decompress <$> BSL.hGet h (fromIntegral $ end - start) 
                                 

{--| Block decompressor wrapper that calls either the chunkDecompress
     or the blockDecompress --}
smartBlockDecompress :: Handle -> Integer -> Integer -> IO BSL.ByteString
smartBlockDecompress h start end 
    | start == end = chunkDecompress h start
    | otherwise = blockDecompress h start end
