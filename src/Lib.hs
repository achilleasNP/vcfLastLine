{-# LANGUAGE OverloadedStrings #-}
module Lib
(runProgram)
where
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary.Get as G
import qualified Codec.Compression.GZip as GZip
import System.FilePath.Posix ( (<.>) )
import System.Directory
import System.IO
import Data.Int
import Data.Word
import Data.Bits



data VirtualFileOffset = VirtualFileOffset {compressedFileOffset :: Integer, chunkOffset :: Int64}

virtualOffset :: Word64 -> VirtualFileOffset
virtualOffset w = VirtualFileOffset fileOffset chunkOffset 
                  where
                      mask = shiftR (complement zeroBits) 48
                      fileOffset = fromIntegral $ shiftR w 16
                      chunkOffset = fromIntegral $ mask .&. w



-- We start by dropping the fixed length part of the header
-- since we are interested only at the linear index.
-- The fixed header consists of:
--    magic: 4  bytes
--    n_ref: Number of reference sequences (int32  = 4 bytes) 
--    format: 0 generic, 1 SAM, 2 VCD (int32 = 4 bytes)
--    col_seq: column for sequence name (int32 = 4 bytes)  
--    col_beg: column for begining of a region  (int32 = 4 bytes)  
--    col_end: column for end of a region  (int32 = 4 bytes)  
--    meta: Leading character of comment lines (int32 = 4 bytes)  
--    skip: Lines to skp at begining of file (int32 = 4 bytes)  
dropTabixHeader :: L8.ByteString -> L8.ByteString
dropTabixHeader s  = case  L8.take 4 s of
                 "TBI\1" -> L8.drop 32 s 
                 _ -> error "blah"



decodeNames :: G.Get ()
decodeNames =  do
                  lengthNames <- fromIntegral <$> G.getInt32le
                  G.skip lengthNames



decodeBin :: G.Get ()
decodeBin  = do
                _ <- G.getWord32le 
                numChunks <- fromIntegral <$> G.getInt32le 
                G.skip $ numChunks * 16 
                


decodeBins :: G.Get()
decodeBins = do
                nBins <- fromIntegral <$> G.getInt32le
                mapM_ (const  decodeBin) [1..nBins] 



getLastInterval :: G.Get Word64 
getLastInterval = do
                    nIntervals <- fromIntegral <$> G.getInt32le
                    G.skip $ (nIntervals-1) * 8 
                    G.getWord64le


completeTabixParser :: G.Get Word64
completeTabixParser = do
                           decodeNames
                           decodeBins
                           getLastInterval
                         


getInfoFromTabix :: FilePath -> IO VirtualFileOffset
getInfoFromTabix tabixFilename = do
                              contents <- dropTabixHeader . GZip.decompress <$> L8.readFile tabixFilename
                              return . virtualOffset $ G.runGet completeTabixParser contents



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
