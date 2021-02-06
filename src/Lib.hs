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

--   Command line interface args
data Args = Args { vcfFile :: FilePath,
                   allSequences :: Bool}

--  CLI parser
args = Args <$> 
         argument str (metavar "VCF_FILE") <*>
         switch (long "all_sequences" <>
                 short 'a'<>
                 help "output lines for the last position in each sequence")
                
--  INFO parser
argsParserInfo = info (args <**> helper)
                   ( fullDesc         
                     <> progDesc "Get the last line of a  tabix indexed vcf file that contains single seqence .e.g. chr1 ") 


--  Main program runner
runProgram :: Args -> IO()
runProgram args = do 
                 let 
                    vcfFilename = vcfFile args
                    tabixFilename  = vcfFilename <.> "tbi"
                    allSeqlLastLines voffs h =  mapM (getLastLine h) voffs
                    lastSeqLastLine voffs h =  (getLastLine h) (last voffs) >>= \x -> return [x]
                    obtainLines = if allSequences args then allSeqlLastLines else lastSeqLastLine

                 tabixfileExists <- doesFileExist tabixFilename
                 if tabixfileExists then
                    do
                        virtualOffsets <- getInfoFromTabix tabixFilename
                        output <- withFile vcfFilename ReadMode   (obtainLines virtualOffsets)
                        L8.putStr . L8.unlines $ output
                 else
                    error $ "Can't find file: " ++ vcfFilename


--  Returns a list of pairs with the sequence names in the file and 
--  the virtual file offsets of the last chunk of the sequence
getInfoFromTabix :: FilePath -> IO [(L8.ByteString, VirtualFileOffset)]
getInfoFromTabix tabixFilename = do
                              contents <- readTabixFile tabixFilename
                              let 
                                  info = runGet completeTabixParser contents
                                  pairs = map (\(x,y) -> (x,virtualOffset y)) info
                              return pairs



--  Obtains the last line in the virtual file offsets chunk for the given seqName. 
getLastLine :: Handle -> (L8.ByteString, VirtualFileOffset) -> IO L8.ByteString
getLastLine h (seqName, voff) = do
                                  let fileOffset = compressedFileOffset voff
                                  contents <- chunkDecompress h fileOffset
                                  return . last . filter (L8.isPrefixOf seqName)  . L8.lines . L8.drop  (chunkOffset voff) $ contents




