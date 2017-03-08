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
    <|> try pArithExpr
    <|> try pTerm


pTerm :: Parser Expr
pTerm = try (parens pExpr)
    <|> try pLambda
    <|> try pNull
    <|> try pUndefined
    <|> try pNothing
    <|> try pString
    <|> try pFloat
    <|> try pInt
    <|> try pBool
    <|> try pVar
    <|> try pList
    <|> try pRecord


pNull :: Parser Expr
pNull = do
    rWord "null"
    return $ ExprLit $ LitNull


pUndefined :: Parser Expr
pUndefined = do
    rWord "undefined"
    return $ ExprLit $ LitUndefied


pNothing :: Parser Expr
pNothing = do
    _ <- sym "()"
    return $ ExprLit $ LitNothing


pString :: Parser Expr
pString = do
    _   <- sym "\""
    str <- P.many $ P.noneOf ['"']
    _   <- sym "\""
    return $ ExprLit $ LitString str


pInt :: Parser Expr
pInt = do
    val <- lexeme $ L.signed sc integer
    return $ ExprLit $ LitInt val


pBool :: Parser Expr
pBool = do
    val <- P.choice [t, f]
    return $ ExprLit $ LitBool val
  where
    t = do rWord "True"; return True
    f = do rWord "False"; return False


pFloat :: Parser Expr
pFloat = do
    val <- lexeme $ L.signed sc float
    return $ ExprLit $ LitFloat val


pList :: Parser Expr
pList = L.lineFold scn $ \sc' -> do
    vals <- brackets $ P.sepBy (manyFolds sc' pExpr) $ sym ","
    return $ ExprLit $ LitList vals


pRecord :: Parser Expr
pRecord = L.lineFold scn $ \sc' -> do
    kvPairs <- braces $ P.sepBy (manyFolds sc' $ pKv sc') $ sym ","
    return $ ExprLit $ LitRecord kvPairs
  where
    pKv sc' = do
        key <- ident
        _   <- sym "="
        _   <- sc'
        val <- pExpr
        return (key, val)


pApp :: Parser Expr
pApp = do
    (call, args) <- lineSepIndentFold pCallable pTerm sc
    return $ ExprApp call args
  where
    pCallable = try (parens pExpr) <|> try pVar


pVar :: Parser Expr
pVar = do
    var <- dotIdent
    return $ ExprVar var


pLambda :: Parser Expr
pLambda = do
    _      <- sym "\\"
    params <- P.many ident
    _      <- sym "->"
    expr   <- pExpr
    return $ ExprLambda params expr


pArithOp :: [[Operator Parser Expr]]
pArithOp =
    [ [ InfixL (sym "*" *> pure (ExprArith ArithOpMultiply))
      , InfixL (sym "/" *> pure (ExprArith ArithOpDivide))
      , InfixL (sym "%" *> pure (ExprArith ArithOpModulus))
      , InfixL (sym "^" *> pure (ExprArith ArithOpPow))
      ]
    , [ InfixL (sym "+" *> pure (ExprArith ArithOpAdd))
      , InfixL (sym "-" *> pure (ExprArith ArithOpSubtract))
      ]
    ]

pArithExpr :: Parser Expr
pArithExpr = makeExprParser pTerm pArithOp
