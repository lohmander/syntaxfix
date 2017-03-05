{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Javascript.Transformer where


import           AST
import           CodeGen.Javascript.AST
import           GHC.Float


class JSTransformable a b where
    transform :: a -> b


instance JSTransformable Module JSModule where
    transform (Module _ _ decls) = JSModule $ map transform decls


instance JSTransformable Decl JSDecl where
    transform (DeclFunc name params statements) = JSDeclFunc name params $ transform statements


instance JSTransformable [Expr] [JSState] where
    transform exprs =
        map transform inits ++ [JSStateReturn $ transform final]
      where
        inits = init exprs
        final = last exprs


instance JSTransformable Expr JSState where
    transform expr = JSStateLoose $ transform expr


instance JSTransformable Expr JSExpr where
    transform (ExprLit lit)         = JSExprLit $ transform lit
    transform (ExprApp fnName args) = JSExprApp fnName $ map transform args


instance JSTransformable Lit JSLit where
    transform (LitString str) = JSLitString str
    transform (LitFloat num)  = JSLitFloat $ double2Float num
    transform _               = JSLitString "NOT IMPLEMENTED"
