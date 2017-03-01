module Parser.Declaration where

import           Text.Megaparsec        (try, (<|>))
import qualified Text.Megaparsec        as P
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

import           AST
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
pFunc = L.nonIndented scn p
  where
    p = do
        fnName <- ident
        _ <- P.some ident
        _ <- sym "="
        return $ DeclFunc fnName $ ExprLit $ LitString "test"
