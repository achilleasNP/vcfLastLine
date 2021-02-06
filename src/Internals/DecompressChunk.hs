module Internals.DecompressChunk
(
chunkDecompress
)
where
import qualified Codec.Compression.Zlib.Internal as GZ
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO
import Control.Exception

{-- * A custom decompressor so that we decompress only one chunk at a time
 - and not the whole file. The decompress uses the option GZ.decompressAllMembers   
 - to force the decompression only of the current chunk.
 --}
myParams = GZ.defaultDecompressParams {GZ.decompressAllMembers = False}


chunkDecompress ::  Handle  -> Integer -> IO BSL.ByteString
chunkDecompress h pos = do
                              hSeek h AbsoluteSeek pos
                              let decompressor = GZ.decompressIO GZ.gzipFormat myParams
                              BSL.fromChunks <$> auxDecompress decompressor h 


auxDecompress :: GZ.DecompressStream IO -> Handle -> IO [BS.ByteString]
auxDecompress (GZ.DecompressInputRequired f) h = do
                                                  s <- BS.hGet h 4096
                                                  res <- f s
                                                  auxDecompress res h

auxDecompress (GZ.DecompressOutputAvailable b next) h = do
                                                         res <- next
                                                         bs <- auxDecompress res h
                                                         return (b:bs)

                                                         
auxDecompress (GZ.DecompressStreamEnd _) _ = return []

auxDecompress (GZ.DecompressStreamError e) _ = throw e
