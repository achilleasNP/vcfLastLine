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





getInfoFromTabix :: FilePath -> IO VirtualFileOffset
getInfoFromTabix tabixFilename = do
                              contents <- GZip.decompress <$> L8.readFile tabixFilename
                              return . virtualOffset $ runGet completeTabixParser contents



getLastLine :: FilePath -> VirtualFileOffset -> IO()
getLastLine vcfFile voff = do
                               fileExists <- doesFileExist vcfFile
                               h <- if fileExists then openFile vcfFile ReadMode
                                    else error "File does not exist"
                               hSeek h AbsoluteSeek $ compressedFileOffset voff 
                               contents <- GZip.decompress <$> L8.hGetContents h  
                               L8.putStrLn . last . L8.lines . L8.drop  (chunkOffset voff) $ contents



runProgram :: FilePath -> IO()
runProgram vcfFile = do 
                 let tabixFilename  = vcfFile <.> "tbi"
                 fileExists <- doesFileExist tabixFilename
                 if fileExists then
                    do
                        virtualOffset <- getInfoFromTabix tabixFilename
                        getLastLine vcfFile virtualOffset
                 else
                    error $ "Can't find file: " ++ vcfFile




