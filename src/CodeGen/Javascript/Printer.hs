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


importStatement :: String -> Maybe String -> [(Maybe String, String)] -> Doc
importStatement src default' imports =
    text "import" <+>
    defaultDoc default' <+>
    braces (text interspersedImports) <+>
    text "from" <+>
    quotes (text src) <>
    semi
  where
    importMap (Just name, import') = name ++ " as " ++ import'
    importMap (Nothing, import')   = import'

    defaultDoc (Just name) = text name <> comma
    defaultDoc (Nothing)   = empty

    interspersedImports = foldl1 (++) $ intersperse ", " $ map importMap imports

commaSep :: [Doc] -> [Doc]
commaSep docs = intersperse (text ", ") docs


toDoc :: PrintableJS a => a -> Doc
toDoc ast = empty &> ast


blank :: Doc
blank = text ""


class PrintableJS a where
    (&>) :: Doc -> a -> Doc


instance PrintableJS JSModule where
    (&>) doc (JSModule decls runs) = doc <> foldl (&>) empty decls <> foldl (&>) empty runs


instance PrintableJS JSDecl where
    (&>) doc (JSDeclFunc name params exprs)      = doc $+$ (funcDef name params $ toDoc exprs) $+$ blank
    (&>) doc (JSDeclExport name)                 = doc $+$ text "export" <+> text name <> semi
    (&>) doc (JSDeclConst name val)              = doc $+$ text "const" <+> text name <+> equals &> val
    (&>) doc (JSDeclImport src default' imports) = doc $+$ importStatement src default' imports


instance PrintableJS [JSState] where
    (&>) doc statements = foldl (&>) empty statements


instance PrintableJS JSComp where
    (&>) doc (JSCompEq e1 e2) = doc &> e1 <+> text "===" &> e2


instance PrintableJS JSState where
    (&>) doc (JSStateReturn expr)         = doc $+$ text "return" &> expr <> semi
    (&>) doc (JSStateLoose expr)          = doc $+$ toDoc expr <> semi
    (&>) doc (JSStateLetAssign name expr) = doc $+$ (letAssign name $ toDoc expr)
    (&>) doc (JSStateIf (x:xs))           =
        doc $+$
        ifState "if" x <+>
        foldl (\doc' x' -> doc' <+> (ifState "else if" x')) empty xs
      where
        ifState kind (cond, state) =
            empty <>
            text kind <+>
            parens (toDoc cond) <+>
            lbrace $+$
            (toDoc state) $+$
            rbrace


instance PrintableJS JSExpr where
    (&>) doc (JSExprLit lit)            = doc &> lit
    (&>) doc (JSExprApp call args)      = doc <+> (funcApp (toDoc call) $ map toDoc args)
    (&>) doc (JSExprVar var)            = doc <+> text var
    (&>) doc (JSExprParensProp expr p)  = doc <+> parens (toDoc expr) <> text "." <> text p
    (&>) doc (JSExprArith op e1 e2)     = doc &> e1 &> op &> e2
    (&>) doc (JSExprLambda params expr) = doc <+> parens (funcDef "" params $ toDoc $ JSStateReturn expr)


instance PrintableJS JSOp where
    (&>) doc (JSOpAdd)      = doc <+> text "+"
    (&>) doc (JSOpSubtract) = doc <+> text "-"
    (&>) doc (JSOpMultiply) = doc <+> text "*"
    (&>) doc (JSOpDivide)   = doc <+> text "/"
    (&>) doc (JSOpModulus)  = doc <+> text "%"


instance PrintableJS JSLit where
    (&>) doc JSLitNothing      = doc
    (&>) doc JSLitNull         = doc <+> text "null"
    (&>) doc JSLitUndefined    = doc <+> text "undefined"
    (&>) doc (JSLitString str) = doc <+> (quotes $ text str)
    (&>) doc (JSLitFloat num)  = doc <+> float num
    (&>) doc (JSLitInt num)    = doc <+> integer num
    (&>) doc (JSLitBool bool)  = doc <+> text (if bool then "true" else "false")
    (&>) doc (JSLitArray vals) = doc <+> (brackets $ foldl (<>) empty $ commaSep $ map toDoc vals)
    (&>) doc (JSLitObject kvs) = doc <+> (braces $ foldl (<>) empty $ commaSep $ map (\(k, v) -> text k <> colon <+> toDoc v) kvs)
