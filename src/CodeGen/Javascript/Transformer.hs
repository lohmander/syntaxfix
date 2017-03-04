{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Javascript.Transformer where


import           AST
import           CodeGen.Javascript.AST


class JSTransformable a b where
    transform :: a -> b


instance JSTransformable Module JSModule where
    transform (Module _ _ decls) = JSModule $ map transform decls


instance JSTransformable Decl JSDecl where
    transform (DeclFunc name params statements) = JSDeclFunc name params $ map transform statements


instance JSTransformable Expr JSState where
    transform expr = JSStateReturn $ transform expr


instance JSTransformable Expr JSExpr where
    transform (ExprLit lit) = JSExprLit $ transform lit


instance JSTransformable Lit JSLit where
    transform (LitString str) = JSLitString str
    transform (LitFloat _)    = JSLitFloat 1.0
    transform _               = JSLitString "NOT IMPLEMENTED"
