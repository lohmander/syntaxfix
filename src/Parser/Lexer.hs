{-# LANGUAGE TupleSections #-}

module Parser.Lexer where

import           Control.Applicative    (empty)
import           Control.Monad          (void)

import           Text.Megaparsec        ((<|>))
import qualified Text.Megaparsec        as P
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String


reservedWords :: [String]
reservedWords =
    [ "from"
    , "fromjs"
    , "import"
    , "default"
    , "as"
    , "class"
    , "extends"
    , "module"
    , "exports"
    , "runs"
    , "const"
    , "where"
    , "True"
    , "False"
    , "None"
    , "undefined"
    ]



scn :: Parser ()
scn = L.space (void P.spaceChar) empty empty


sc :: Parser ()
sc = L.space (void $ P.oneOf " \t") empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


rWord :: String -> Parser ()
rWord w = P.string w *> P.notFollowedBy P.alphaNumChar *> sc


ident :: Parser String
ident = identWith ['_']


dotIdent :: Parser String
dotIdent = identWith ['_', '.']


identWith :: [Char] -> Parser String
identWith chars = lexeme (p >>= check)
  where
    p = (:) <$> P.letterChar <*> P.many (foldl (\acc ch -> acc <|> P.char ch) P.alphaNumChar chars)
    check x =
        if x `elem` reservedWords
            then fail $ "Keyword " ++ show x ++ " is reserved."
            else return x


sym :: String -> Parser String
sym = L.symbol sc


integer :: Parser Integer
integer = lexeme L.integer


float :: Parser Double
float = lexeme L.float


parens :: Parser a -> Parser a
parens = P.between (sym "(") (sym ")")


brackets :: Parser a -> Parser a
brackets = P.between (sym "[") (sym "]")


braces :: Parser a -> Parser a
braces = P.between (sym "{") (sym "}")


manyFolds :: Parser () -> Parser a -> Parser a
manyFolds sc' p = do
    x <- p
    _ <- P.try sc' <|> scn
    return x


lineSepIndentFold :: Parser a -> Parser b -> Parser c -> Parser (a, [b])
lineSepIndentFold p ps sep = P.try pOneLine <|> L.indentBlock scn pMultiLine
  where
    pOneLine = do
        x <- p
        y <- P.sepBy1 ps sep
        return (x, y)

    pMultiLine = do
        x <- p
        return $ L.IndentSome Nothing (return . (x,)) ps
