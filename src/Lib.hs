module Lib
(runProgram)
where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import  Data.Binary.Get (runGet)
import qualified Codec.Compression.GZip as GZip
import Internals.VirtualFileOffset
import Internals.TabixParser
import System.FilePath.Posix ( (<.>) )
import System.Directory
import System.IO





getInfoFromTabix :: FilePath -> IO [(L8.ByteString, VirtualFileOffset)]
getInfoFromTabix tabixFilename = do
                              contents <- readTabixFile tabixFilename
                              let 
                                  info = runGet completeTabixParser contents
                                  pairs = map (\(x,y) -> (x,virtualOffset y)) info
                              return pairs



getLastLine :: FilePath -> (L8.ByteString, VirtualFileOffset) -> IO L8.ByteString
getLastLine vcfFile (seqName, voff) = do
                               fileExists <- doesFileExist vcfFile
                               h <- if fileExists then openFile vcfFile ReadMode
                                    else error "File does not exist"
                               hSeek h AbsoluteSeek $ compressedFileOffset voff 
                               contents <- GZip.decompress <$> L8.hGetContents h  
                               return . last . filter (L8.isPrefixOf seqName)  . L8.lines . L8.drop  (chunkOffset voff) $ contents



runProgram :: FilePath -> IO()
runProgram vcfFile = do 
                 let tabixFilename  = vcfFile <.> "tbi"
                 fileExists <- doesFileExist tabixFilename
                 if fileExists then
                    do
                        virtualOffsets <- getInfoFromTabix tabixFilename
                        lastLines <-  mapM (getLastLine vcfFile) virtualOffsets
                        L8.putStrLn . last $ lastLines

                 else
                    error $ "Can't find file: " ++ vcfFile




