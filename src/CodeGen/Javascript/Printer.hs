{-# LANGUAGE FlexibleInstances #-}

module CodeGen.Javascript.Printer where

import           CodeGen.Javascript.AST
import           Data.List
import           Text.PrettyPrint


data JSDoc = Doc


print :: JSModule -> String
print mod = render $ empty &> mod


funcDef :: String -> [String] -> Doc -> Doc
funcDef name params body =
    text "function" <+>
    text name <>
    parens (text $ foldl (++) "" $ intersperse ", " params) <+>
    lbrace $+$
    nest 4 body $+$
    rbrace


funcApp :: Doc -> [Doc] -> Doc
funcApp call args =
    call <>
    parens (foldl (<>) empty $ intersperse (text ", ") args)


letAssign :: String -> Doc -> Doc
letAssign name val =
    text "let" <+>
    text name <+>
    equals <+>
    val <>
    semi


commaSep :: [Doc] -> [Doc]
commaSep docs = intersperse (text ", ") docs


toDoc :: PrintableJS a => a -> Doc
toDoc ast = empty &> ast


class PrintableJS a where
    (&>) :: Doc -> a -> Doc


instance PrintableJS JSModule where
    (&>) doc (JSModule decls) = doc <> foldl (&>) empty decls


instance PrintableJS JSDecl where
    (&>) doc (JSDeclFunc name params exprs) = doc $+$ (funcDef name params $ toDoc exprs)
    (&>) doc (JSDeclExport name)            = doc $+$ text "export" <+> text name <> semi


instance PrintableJS [JSState] where
    (&>) doc statements = foldl (&>) empty statements


instance PrintableJS JSState where
    (&>) doc (JSStateReturn expr) = doc $+$ text "return" &> expr <> semi
    (&>) doc (JSStateLoose expr)  = doc $+$ toDoc expr <> semi
    (&>) doc (JSStateLetAssign name expr) = doc $+$ (letAssign name $ toDoc expr)


instance PrintableJS JSExpr where
    (&>) doc (JSExprLit lit)        = doc &> lit
    (&>) doc (JSExprApp call args)  = doc <+> (funcApp (toDoc call) $ map toDoc args)
    (&>) doc (JSExprVar var)        = doc <+> text var
    (&>) doc (JSExprArith op e1 e2) = doc &> e1 &> op &> e2


instance PrintableJS JSOp where
    (&>) doc (JSOpAdd)      = doc <+> text "+"
    (&>) doc (JSOpSubtract) = doc <+> text "-"
    (&>) doc (JSOpMultiply) = doc <+> text "*"
    (&>) doc (JSOpDivide)   = doc <+> text "/"
    (&>) doc (JSOpModulus)   = doc <+> text "%"


instance PrintableJS JSLit where
    (&>) doc (JSLitString str) = doc <+> (quotes $ text str)
    (&>) doc (JSLitFloat num)  = doc <+> float num
    (&>) doc (JSLitInt num)    = doc <+> integer num
    (&>) doc (JSLitBool bool)  = doc <+> text (if bool then "true" else "false")
    (&>) doc (JSLitArray vals) = doc <+> (brackets $ foldl (<>) empty $ commaSep $ map toDoc vals)
