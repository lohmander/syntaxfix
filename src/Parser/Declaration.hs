{-# LANGUAGE TupleSections #-}

module Parser.Declaration where

import           Text.Megaparsec        (try, (<|>), (<?>))
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
    exports <- P.option [] $ try $ p "exports"
    runs    <- P.option [] $ try $ p "runs"
    return $ Module moduleName exports runs []
  where
    p rw = do
        rWord rw
        exports <- P.some ident
        return exports



pDecl :: Parser Decl
pDecl = try pImport
    <|> try pFunc


pImport :: Parser Decl
pImport = L.nonIndented scn $ do
    kind    <- try pFromJS <|> pFromSF
    src     <- identWith ['/']
    rWord "import"
    default' <- P.option Nothing $ try $ parens pDefaultAs
    imports  <- P.sepBy1 (try (parens pImportItemAs) <|> pImportItem) sc
    return $ DeclImport kind src default' imports
  where
    pFromJS = do rWord "fromjs"; return ImportJS
    pFromSF = do rWord "from"; return ImportSyntaxfix

    pDefaultAs = do
        rWord "default";
        rWord "as"
        name <- ident
        return $ Just name

    pImportItem = do
        name <- ident
        return (Nothing, name)

    pImportItemAs = do
        imported <- ident
        rWord "as"
        name     <- ident
        return (Just imported, name)


pFunc :: Parser Decl
pFunc = L.nonIndented scn $ do
    pos  <- L.indentLevel

    (fnName, params) <- pFuncDef
    body             <- pFuncBody pos
    vars             <- P.option [] (try $ pFuncVars pos)

    return $ DeclFunc fnName params body vars


pFuncDef :: Parser (String, [String])
pFuncDef = do
    fnName <- ident
    params <- P.many ident
    _      <- sym "="
    return (fnName, params)


pFuncBody :: P.Pos -> Parser [Expr]
pFuncBody pos = do
    exprs <- P.some $ try (scn *> levelCheck *> pExpr)
    return exprs
  where
    levelCheck = L.indentGuard sc GT pos


pFuncVars :: P.Pos -> Parser [(String, Expr)]
pFuncVars pos = do
    scn
    _ <- levelCheck
    rWord "where"
    vars <- P.some $ try (scn *> levelCheck *> pAssign)
    return vars
  where
    levelCheck = L.indentGuard scn GT pos
    pAssign    = do
        var <- ident
        _   <- sym "="
        val <- pExpr
        return (var, val)
