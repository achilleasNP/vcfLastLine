{-# LANGUAGE OverloadedStrings #-}
module Internals.TabixParser
(
  completeTabixParser,
  readTabixFile,
)


where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Binary.Get as G
import qualified Codec.Compression.GZip as GZip
import Data.Int
import Data.Word
import Data.Bits
import Data.Char (ord, chr)


readTabixFile:: FilePath -> IO L8.ByteString
readTabixFile tabixFilename = GZip.decompress <$> L8.readFile tabixFilename

{-- Basic tabix parser functions. Description of the tabix file format can
 -  be found at: https://samtools.github.io/hts-specs/tabix.pdf
 - A  description  of the fixed header is here:
-- The fixed header consists of:
--    magic: 4  bytes
--    n_ref: Number of reference sequences (int32  = 4 bytes) 
--    format: 0 generic, 1 SAM, 2 VCD (int32 = 4 bytes)
--    col_seq: column for sequence name (int32 = 4 bytes)  
--    col_beg: column for begining of a region  (int32 = 4 bytes)  
--    col_end: column for end of a region  (int32 = 4 bytes)  
--    meta: Leading character of comment lines (int32 = 4 bytes)  
--    skip: Lines to skp at begining of file (int32 = 4 bytes)
--}


-- Parse fixed header of tabix file and get numer of sequences. 
decodeNReferences :: G.Get Int
decodeNReferences = do
                      fileMagic <- mapM (const G.getWord8) [1..4]
                      let tabixMagic = map (fromIntegral . ord ) "TBI\1"
                      nRef <- if fileMagic == tabixMagic
                              then G.getInt32le 
                              else error "Not a tabix file"
                      G.skip 24
                      return (fromIntegral nRef)
                         
                    
-- Parse the part of the file that contains the name of the
-- available sequences. 
decodeRefNames :: G.Get [L8.ByteString]
decodeRefNames =  do
                  lengthNames <- fromIntegral <$> G.getInt32le
                  names <- L8.split '\0' . L8.pack . init  <$> mapM (const  (chr . fromIntegral <$> G.getWord8)) [1..lengthNames] 
                  return names


-- Skip one bin
decodeBin :: G.Get ()
decodeBin  = do
                _ <- G.getWord32le 
                numChunks <- fromIntegral <$> G.getInt32le 
                G.skip $ numChunks * 16 
                


-- Skip all bins
decodeBins :: G.Get()
decodeBins = do
                nBins <- fromIntegral <$> G.getInt32le
                mapM_ (const  decodeBin) [1..nBins] 



-- Get virtual file offset for last interval
getLastInterval :: G.Get Word64 
getLastInterval = do
                    nIntervals <- fromIntegral <$> G.getInt32le
                    G.skip $ (nIntervals-1) * 8 
                    G.getWord64le



--Combine all parsers to get pairs of available sequence names
--and the virtual offset for the last chunk of the sequence
completeTabixParser :: G.Get [(L8.ByteString, Word64)]
completeTabixParser = do
                           nRef <- decodeNReferences
                           namesRef <- decodeRefNames
                           chunkLastRefs <- mapM  (const (decodeBins >> getLastInterval))  namesRef
                           return $ zip namesRef chunkLastRefs
