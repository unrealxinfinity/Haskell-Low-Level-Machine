module Main where

import Parser
import Interpreter
import System.IO
import System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs  
    case args of
        [filename] -> do
            fileContent <- readFile filename
            case testParser fileContent of
                (stack, state) -> putStrLn (show state)
        _ -> putStrLn "Usage: programName filename"
