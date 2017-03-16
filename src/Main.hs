{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CodeGen.Javascript.Printer     as JS
import qualified CodeGen.Javascript.Transformer as JS
import           Desugar
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
run (Compile filename) = readFile filename >>= putStr . (gen filename)
run _ = do putStr "Can't do that yet."


gen :: String -> String -> String
gen filename c = case (parse parser filename c) of
    Left err -> parseErrorPretty err
    Right st -> case desugar st of
        Left err  -> show err
        Right st' -> JS.print $ JS.transform st'
