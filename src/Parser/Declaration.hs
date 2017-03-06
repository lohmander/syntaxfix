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
pDecl = try pFunc


-- pFunc :: Parser Decl
-- pFunc = L.nonIndented scn $ do
--     exprs  <- pMultiLine
--     -- vars   <- P.option [] pWhere
--     return $ DeclFunc fnName params exprs []
--   where
--     pDef = do
--         fnName <- ident
--         params <- P.many ident
--         _      <- sym "="
        

--     pAssign = do
--         var  <- ident
--         _    <- sym "="
--         expr <- pExpr
--         return (var, expr)

--     pWhere = L.indentBlock scn $ do
--         rWord "where"
--         return $ L.IndentSome Nothing return pAssign

--     pOneLine = do
--         exprs <- P.sepBy1 pExpr $ sym ";"
--         return exprs

--     pMultiLine = L.indentBlock scn $ do
--         return $ L.IndentSome (Just 5) return pExpr


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


-- pFunc :: Parser Decl
-- pFunc = L.nonIndented scn $ do
--     (DeclFunc fnName params exprs _) <- (try pOneLine <|> pMultiLine)
--     -- vars                             <- P.option [] pWhere
--     return $ DeclFunc fnName params exprs []
--   where
--     pDef = do
--         fnName <- ident
--         params <- P.many ident
--         _      <- sym "="
--         return (fnName, params)

--     pAssign = do
--         var  <- ident
--         _    <- sym "="
--         expr <- pExpr
--         return (var, expr)

--     pWhere = L.lineFold scn $ \sc' -> do
--         rWord "where"
--         sc'
--         vars <- P.some $ manyFolds sc' pAssign
--         return vars

--     pOneLine = do
--         (fnName, params) <- pDef
--         exprs            <- P.sepBy1 pExpr $ sym ";"
--         return $ DeclFunc fnName params exprs []

--     pMultiLine = L.indentBlock scn $ do
--         (fnName, params) <- pDef
--         return $ L.IndentSome Nothing (return . (\exprs -> DeclFunc fnName params exprs [])) pExpr
