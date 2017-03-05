module Main where

import qualified CodeGen.Javascript.Printer as JS
import qualified CodeGen.Javascript.Transformer as JS
import           Parser
import           Text.Megaparsec

import           System.Environment
import           System.Exit


main = getArgs >>= cmdParse >>= putStr . gen


cmdParse ["-h"] = usage   >> exit
cmdParse ["-v"] = version >> exit
cmdParse []     = getContents
cmdParse fs     = concat `fmap` mapM readFile fs


usage   = putStrLn "Usage: syntaxfix [-vh] [file...]"
version = putStrLn "Syntaxfix 0.0"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)


gen :: String -> String
gen c = case (parse parser "./test.sf" c) of
    Left err -> parseErrorPretty err
    Right st -> JS.print $ JS.transform st
