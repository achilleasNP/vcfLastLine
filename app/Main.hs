module Main where

import Lib
import Options.Applicative

newtype Args = Args { vcfFile :: FilePath }

args = Args <$> argument str (metavar "VCF_FILE") 
argsParserInfo = info (args <**> helper)
                   ( fullDesc         
                     <> progDesc "Get the last line of a  tabix indexed vcf file that contains single seqence .e.g. chr1 ") 

main :: IO ()
main = do
          curArgs <- execParser argsParserInfo
          runProgram $ vcfFile curArgs
            
