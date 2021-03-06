{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CodeGen.Javascript.Transformer where


import           AST
import           CodeGen.Javascript.AST
import           GHC.Float


makeFnCond :: Decl -> (JSComp, JSState)
makeFnCond (DeclFunc name pc params (x:_) vars) =
    (cond, JSStateReturn $ JSExprApp fn $ map (\n -> JSExprVar $ "arguments[" ++ (show n) ++ "]") [0..pc - 1])
  where
    cond = JSCompEq (JSExprParensProp (JSExprVar "arguments") "length") (JSExprLit $ JSLitInt $ toInteger pc)
    fn   = JSExprLambda params $ transform x


class JSTransformable a b where
    transform :: a -> b


instance JSTransformable Module JSModule where
    transform (Module _ exports runs decls) =
        JSModule
            ((map transform decls) ++ (map (\e -> JSDeclExport e) exports))
            (map (\name -> JSStateLoose $ JSExprApp (JSExprVar name) []) runs)


instance JSTransformable Decl JSDecl where
    transform (DeclOverloadedFunc name fns)            = JSDeclFunc name [] [JSStateIf $ map makeFnCond fns] -- map nestFn fns
    transform (DeclFunc name _ params statements vars) = JSDeclFunc name params $ transform vars ++ transform statements
    transform (DeclImport _ src default' imports)      = JSDeclImport src default' imports
    transform (DeclConst name val)                     = JSDeclConst name $ transform val


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
    transform (ExprLit lit)                = JSExprLit $ transform lit
    transform (ExprApp call args)          = JSExprApp (transform call) $ map transform args
    transform (ExprVar var)                = JSExprVar var
    transform (ExprArith ArithOpPow e1 e2) = JSExprApp (JSExprVar "Math.pow") $ map transform [e1, e2]
    transform (ExprArith op e1 e2)         = JSExprArith (transform op) (transform e1) (transform e2)
    transform (ExprLambda params expr)     = JSExprLambda params $ transform expr
    transform (ExprParensProp expr prop)   = JSExprParensProp (transform expr) prop
    transform (ExprPipe _ _)               = JSExprVar "NOT IMPLMENTED"


instance JSTransformable ArithOp JSOp where
    transform ArithOpAdd      = JSOpAdd
    transform ArithOpSubtract = JSOpSubtract
    transform ArithOpMultiply = JSOpMultiply
    transform ArithOpDivide   = JSOpDivide
    transform ArithOpModulus  = JSOpModulus
    transform _               = JSOpAdd -- will never hit


instance JSTransformable Lit JSLit where
    transform (LitString str) = JSLitString str
    transform (LitFloat num)  = JSLitFloat $ double2Float num
    transform (LitList vals)  = JSLitArray $ map transform vals
    transform (LitRecord kvs) = JSLitObject $ map (\(k, v) -> (k, transform v)) kvs
    transform (LitInt num)    = JSLitInt num
    transform (LitBool bool)  = JSLitBool bool
    transform LitNothing      = JSLitNothing
    transform LitNull         = JSLitNull
    transform LitUndefied     = JSLitUndefined

