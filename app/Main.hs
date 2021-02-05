module Main where

import Lib
import Options.Applicative

main :: IO ()
main = do
          curArgs <- execParser argsParserInfo
          runProgram curArgs
            
