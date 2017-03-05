module Parser.Expression where

import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Lexer


pExpr :: Parser Expr
pExpr = try pTerm
    <|> try pApp


pTerm :: Parser Expr
pTerm = try pString
    <|> try pFloat


pString :: Parser Expr
pString = do
    _   <- sym "\""
    str <- P.many (P.alphaNumChar <|> P.spaceChar)
    _   <- sym "\""
    return $ ExprLit $ LitString str


pFloat :: Parser Expr
pFloat = do
    val <- L.signed sc L.float
    return $ ExprLit $ LitFloat val


pApp :: Parser Expr
pApp = do
    fnName <- ident
    sc
    args   <- P.some pExpr
    return $ ExprApp fnName args
