module Main where

import Parser
import Interpreter
import Extra
import System.IO
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs  
    case args of
        [filename] -> do
            fileContent <- readFile filename
            case testParserFile fileContent of
                printData -> putStrLn printData
        _ -> putStrLn "Usage: programName filename"
