module Lib
(runProgram,
argsParserInfo)
where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import  Data.Binary.Get (runGet)
import qualified Codec.Compression.GZip as GZip
import Options.Applicative
import Internals.VirtualFileOffset
import Internals.TabixParser
import Internals.DecompressChunk
import System.FilePath.Posix ( (<.>) )
import System.Directory
import System.IO


data Args = Args { vcfFile :: FilePath,
                   allSequences :: Bool}

args = Args <$> 
         argument str (metavar "VCF_FILE") <*>
         switch (long "all_sequences" <>
                 short 'a'<>
                 help "output lines for the last position in each sequence")
                
argsParserInfo = info (args <**> helper)
                   ( fullDesc         
                     <> progDesc "Get the last line of a  tabix indexed vcf file that contains single seqence .e.g. chr1 ") 




getInfoFromTabix :: FilePath -> IO [(L8.ByteString, VirtualFileOffset)]
getInfoFromTabix tabixFilename = do
                              contents <- readTabixFile tabixFilename
                              let 
                                  info = runGet completeTabixParser contents
                                  pairs = map (\(x,y) -> (x,virtualOffset y)) info
                              return pairs



getLastLine :: Handle -> (L8.ByteString, VirtualFileOffset) -> IO L8.ByteString
getLastLine h (seqName, voff) = do
                                  let fileOffset = compressedFileOffset voff
                                  contents <- chunkDecompress h fileOffset
                                  return . last . filter (L8.isPrefixOf seqName)  . L8.lines . L8.drop  (chunkOffset voff) $ contents



runProgram :: Args -> IO()
runProgram args = do 
                 let 
                    vcfFilename = vcfFile args
                    tabixFilename  = vcfFilename <.> "tbi"
                    allFunction voffs h =    mapM (getLastLine h) voffs
                    singleFunction voffs h =   (getLastLine h) (last voffs) >>= \x -> return [x]
                    outputFunction = if allSequences args then allFunction else singleFunction

                 fileExists <- doesFileExist tabixFilename
                 if fileExists then
                    do
                        virtualOffsets <- getInfoFromTabix tabixFilename
                        output <- withFile vcfFilename ReadMode   (outputFunction virtualOffsets)
                        L8.putStr . L8.unlines $ output
                 else
                    error $ "Can't find file: " ++ vcfFilename

