module Main where

import           Parser
import           Text.Megaparsec

import           System.Environment
import           System.Exit


main = getArgs >>= cmdParse >>= putStr . gen


cmdParse ["-h"] = usage   >> exit
cmdParse ["-v"] = version >> exit
cmdParse []     = getContents
cmdParse fs     = concat `fmap` mapM readFile fs


usage   = putStrLn "Usage: haru [-vh] [file...]"
version = putStrLn "Haru 0.0"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


gen :: String -> String
gen c = case (parse parser "./test.hu" c) of
    Left err -> parseErrorPretty err
    Right st -> show st
