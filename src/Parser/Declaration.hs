module Parser.Declaration where

import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
import           Parser.Expression
import           Parser.Lexer


pModule :: Parser Module
pModule = do
    rWord "module"
    moduleName <- ident
    rWord "exports"
    exports <- p
    return $ Module moduleName exports []
  where
    p = P.sepBy ident sc


pDecl :: Parser Decl
pDecl = try pFunc


pFunc :: Parser Decl
pFunc = L.nonIndented scn $ L.lineFold scn $ \sc' -> do
    fnName <- ident
    params <- P.many ident
    _      <- sym "="
    sc'
    exprs  <- P.some $ pFoldExpr sc'
    return $ DeclFunc fnName params exprs
  where
    pFoldExpr = \sc' -> do
        expr <- pExpr
        _    <- P.try sc' <|> scn
        return expr


-- pFunc :: Parser Decl
-- pFunc = L.lineFold scn $ \sc' -> do
--     fnName <- ident
--     params <- P.many ident
--     _      <- sym "="
--     sc'
--     exprs  <- P.sepBy1 pExpr sc'
--     return $ DeclFunc fnName params exprs
