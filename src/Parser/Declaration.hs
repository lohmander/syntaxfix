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
pFunc = L.nonIndented scn (try pOneLine <|> pMultiLine)
  where
    pDef = do
        fnName <- ident
        params <- P.many ident
        _      <- sym "="
        return (fnName, params)

    pOneLine = do
        (fnName, params) <- pDef
        exprs            <- P.sepBy1 pExpr $ sym ";"
        return $ DeclFunc fnName params exprs

    pMultiLine = L.indentBlock scn $ do
        (fnName, params) <- pDef
        return $ L.IndentSome Nothing (return . (\exprs -> DeclFunc fnName params exprs)) pExpr
