{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CodeGen.Javascript.Printer     as JS
import qualified CodeGen.Javascript.Transformer as JS
import           Parser
import           Text.Megaparsec

import           Data.Version
import           Options.Generic
import           System.Environment
import           System.Exit


data SyntaxFixCLI
    = Compile String
    | Format String
    deriving (Generic, Show)


instance ParseRecord SyntaxFixCLI


main :: IO ()
main = do
    x <- getRecord "SyntaxFix"
    run (x :: SyntaxFixCLI)


run :: SyntaxFixCLI -> IO ()
run (Compile filename) = readFile filename >>= putStr . gen
run _ = do putStr "Can't do that yet."


gen :: String -> String
gen c = case (parse parser "./test.sf" c) of
    Left err -> parseErrorPretty err
    Right st -> JS.print $ JS.transform st
