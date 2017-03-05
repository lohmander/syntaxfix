module Parser.Expression where

import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Lexer


pExpr :: Parser Expr
pExpr = try pApp
    <|> try pTerm


pTerm :: Parser Expr
pTerm = try (parens pExpr)
    <|> try pString
    <|> try pFloat
    <|> try pVar
    <|> try pList


pString :: Parser Expr
pString = do
    _   <- sym "\""
    str <- P.many (P.alphaNumChar <|> P.spaceChar)
    _   <- sym "\""
    return $ ExprLit $ LitString str


pFloat :: Parser Expr
pFloat = do
    val <- lexeme $ L.signed sc float
    return $ ExprLit $ LitFloat val


pList :: Parser Expr
pList = L.lineFold scn $ \sc' -> do
    vals <- brackets $ P.sepBy (manyFolds sc' pExpr) $ sym ","
    return $ ExprLit $ LitList vals


pApp :: Parser Expr
pApp = L.lineFold scn $ \sc' -> do
    fnName <- ident
    sc'
    args   <- P.some $ manyFolds sc' pTerm
    return $ ExprApp fnName args


pVar :: Parser Expr
pVar = do
    var <- ident
    return $ ExprVar var
