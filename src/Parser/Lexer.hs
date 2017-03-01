module Parser.Lexer
    ( scn
    , sc
    , rWord
    , ident
    , sym
    , lexeme
    , integer
    , parens
    ) where

import           Control.Applicative    (empty)
import           Control.Monad          (void)

import           Text.Megaparsec        ((<|>))
import qualified Text.Megaparsec        as P
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String


reservedWords :: [String]
reservedWords =
    [ "from"
    , "import"
    , "class"
    , "module"
    , "True"
    , "False"
    , "None"
    ]


lineComment :: Parser ()
lineComment = L.skipLineComment "#"


scn :: Parser ()
scn = L.space (void P.spaceChar) lineComment empty


sc :: Parser ()
sc = L.space (void $ P.oneOf " \t") lineComment empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


rWord :: String -> Parser ()
rWord w = P.string w *> P.notFollowedBy P.alphaNumChar *> sc


ident :: Parser String
ident = lexeme (p >>= check)
  where
    p = (:) <$> P.letterChar <*> P.many (P.alphaNumChar <|> P.char '_')
    check x =
        if x `elem` reservedWords
            then fail $ "Keyword " ++ show x ++ " is reserved."
            else return x


sym :: String -> Parser String
sym = L.symbol sc


integer :: Parser Integer
integer = lexeme L.integer


parens :: Parser a -> Parser a
parens = P.between (sym "(") (sym ")")