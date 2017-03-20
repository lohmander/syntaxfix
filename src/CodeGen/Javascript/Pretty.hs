{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen.Javascript.Pretty where

import           Control.Monad.State
import           Data.List
import           Text.PrettyPrint

import qualified CodeGen.Javascript.Syntax as JS


data PrinterState
    = PrinterState
    { doc :: Doc
    , ind :: Int
    }
    deriving (Show)


newtype Printer a = Printer { runPrinter :: State PrinterState a }
    deriving (Functor, Applicative, Monad, MonadState PrinterState)


indent :: Printer ()
indent = modify $ \s -> s { ind = 4 + ind s }


dedent :: Printer ()
dedent = modify $ \s -> s { ind = (ind s) - 4 }


append :: Doc -> Printer ()
append doc' = modify $ \s -> s { doc = doc s $+$ nest (ind s) doc' }


append_ :: Doc -> Printer ()
append_ doc' = modify $ \s -> s { doc = doc s <> nest (ind s) doc' }


newline :: Printer ()
newline = append $ text ""


emptyPrinterState :: PrinterState
emptyPrinterState = PrinterState empty 0


execPrinter :: Printer a -> Doc
execPrinter m = doc $ execState (runPrinter m) emptyPrinterState


prettyPrint :: JS.Module -> String
prettyPrint mod' = render $ execPrinter $ pModule mod'


mapIntersperse_ :: Monad m => m b -> (a -> m b) -> [a] -> m ()
mapIntersperse_ _ _ []       = return ()
mapIntersperse_ _ fn (x:[])  = fn x >> return ()
mapIntersperse_ sp fn (x:xs) = do
    _ <- fn x
    _ <- sp
    mapIntersperse_ sp fn xs


----------------------------------------------------------------
-- Parts
----------------------------------------------------------------


func :: String -> [String] -> Doc
func name params =
    empty <>
    text "function" <+>
    text name <>
    parens (hsep $ punctuate comma $ map text params)


export :: String -> Doc
export name =
    empty <>
    text "export" <+>
    text name <>
    semi


return' :: Doc
return' = text "return"


compareEq :: Doc
compareEq = space <> text "===" <> space


commaSpc :: Doc
commaSpc = comma <> space


----------------------------------------------------------------
-- Printers
----------------------------------------------------------------


pModule :: JS.Module -> Printer ()
pModule (JS.Module sts) = do
    mapM_ pStatement sts


pStatement :: JS.Statement -> Printer ()
pStatement st = case st of
    JS.Func name params sts -> do
        append $ (func name params) <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        append rbrace
        newline

    JS.If cases -> do
        pIf cases

    JS.Export name ->
        append $ export name

    JS.Return expr -> do
        append $ return' <> space
        pExpr expr
        append_ semi

    JS.Expr expr -> do
        newline
        pExpr expr
        append_ semi


pIf :: [(JS.Expr, [JS.Statement])] -> Printer ()
pIf ifs = do
    let h = head ifs
    let t = tail ifs

    if_ "if" h
    mapM_ (if_ "else if") t
  where
    if_ if' (cond, sts) = do
        append $ text if' <+> lparen
        pExpr cond
        append_ $ rparen <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        append rbrace


pExpr :: JS.Expr -> Printer ()
pExpr expr = case expr of
    JS.Lit lit ->
        pLit lit

    JS.Var var ->
        append_ $ text var

    JS.App fn args -> do
        append_ lparen
        pExpr fn
        append_ $ rparen <> lparen
        mapIntersperse_ (append_ commaSpc) pExpr args
        append_ rparen

    JS.Lambda params sts -> do
        append_ $ func "" params <+> lbrace
        indent
        mapM_ pStatement sts
        dedent
        append rbrace

    JS.Comp JS.Eq e1 e2 -> do
        pExpr e1
        append_ compareEq
        pExpr e2


pLit :: JS.Lit -> Printer ()
pLit lit = case lit of
    JS.JSString str ->
        append_ $ quotes $ text str

    JS.JSBool bool ->
        append_ $ text $ if bool then "true" else "false"

    JS.JSNumber num ->
        append_ $ text $ show num
