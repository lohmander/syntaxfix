{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Javascript.Transformer where


import           AST
import           CodeGen.Javascript.AST
import           GHC.Float


class JSTransformable a b where
    transform :: a -> b


instance JSTransformable Module JSModule where
    transform (Module _ exports decls) = JSModule $
        (map transform decls) ++ (map (\e -> JSDeclExport e) exports)


instance JSTransformable Decl JSDecl where
    transform (DeclFunc name params statements vars) = JSDeclFunc name params $ transform vars ++ transform statements


instance JSTransformable [(String, Expr)] [JSState] where
    transform assignments = map (\(name, expr) -> JSStateLetAssign name $ transform expr) assignments


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
    transform (ExprApp call args)   = JSExprApp (transform call) $ map transform args
    transform (ExprVar var)         = JSExprVar var
    transform (ExprArith op e1 e2)  = JSExprArith (transform op) (transform e1) (transform e2)
    transform (ExprPipe _ _)        = JSExprVar "NOT IMPLMENTED"


instance JSTransformable ArithOp JSOp where
    transform ArithOpAdd      = JSOpAdd
    transform ArithOpSubtract = JSOpSubtract
    transform ArithOpMultiply = JSOpMultiply
    transform ArithOpDivide   = JSOpDivide
    transform ArithOpModulos  = JSOpAdd


instance JSTransformable Lit JSLit where
    transform (LitString str) = JSLitString str
    transform (LitFloat num)  = JSLitFloat $ double2Float num
    transform (LitList vals)  = JSLitArray $ map transform vals
    transform (LitInt num)    = JSLitInt num
    transform (LitBool bool)  = JSLitBool bool
