{-# LANGUAGE FlexibleInstances #-}

module CodeGen.Javascript.Printer where

import           CodeGen.Javascript.AST
import           Data.List
import           Text.PrettyPrint


data JSDoc = Doc


print :: JSModule -> String
print mod = render $ empty &> mod


funcDef :: String -> [String] -> Doc -> Doc
funcDef fnName params body =
    text "function" <+>
    text fnName <>
    parens (text $ foldl (++) "" $ intersperse ", " params) <+>
    lbrace $+$
    nest 4 body $+$
    rbrace


funcApp :: String -> [Doc] -> Doc
funcApp fnName args =
    text fnName <>
    parens (foldl (<>) empty $ intersperse (text ", ") args)


toDoc :: PrintableJS a => a -> Doc
toDoc ast = empty &> ast


class PrintableJS a where
    (&>) :: Doc -> a -> Doc


instance PrintableJS JSModule where
    (&>) doc (JSModule decls) = doc <> foldl (&>) empty decls


instance PrintableJS JSDecl where
    (&>) doc (JSDeclFunc name params exprs) =
        doc $+$
        text ("function " ++ name) <>
        parens (text $ foldl (++) "" $ intersperse ", " params) <+>
        lbrace $+$
        nest 4 (empty &> exprs) $+$
        rbrace


instance PrintableJS [JSState] where
    (&>) doc statements = foldl (&>) empty statements


instance PrintableJS JSState where
    (&>) doc (JSStateReturn expr) = doc $+$ text "return" &> expr <> semi
    (&>) doc (JSStateLoose expr)  = doc &> expr <> semi


instance PrintableJS JSExpr where
    (&>) doc (JSExprLit lit)         = doc &> lit
    (&>) doc (JSExprApp fnName args) = doc $+$ (funcApp fnName $ map toDoc args)


instance PrintableJS JSLit where
    (&>) doc (JSLitString str) = doc <+> (doubleQuotes $ text str)
    (&>) doc (JSLitFloat num)  = doc <+> float num
