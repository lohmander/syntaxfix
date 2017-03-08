module CodeGen.Javascript.AST where


data JSModule
    = JSModule [JSDecl] [JSState]
    deriving (Show, Eq)


data JSDecl
    = JSDeclFunc String [String] [JSState]
    | JSDeclExport String
    | JSDeclImport String (Maybe String) [(Maybe String, String)]
    | JSDeclConst String JSExpr
    deriving (Show, Eq)


data JSState
    = JSStateReturn JSExpr
    | JSStateLetAssign String JSExpr
    | JSStateLoose JSExpr -- REMOVE THIS
    deriving (Show, Eq)


data JSExpr
    = JSExprLit JSLit
    | JSExprApp JSExpr [JSExpr]
    | JSExprVar String
    | JSExprArith JSOp JSExpr JSExpr
    | JSExprLambda [String] JSExpr
    deriving (Show, Eq)


data JSOp
    = JSOpAdd
    | JSOpSubtract
    | JSOpMultiply
    | JSOpDivide
    | JSOpModulus
    deriving (Show, Eq)

data JSLit
    = JSLitString String
    | JSLitFloat Float
    | JSLitInt Integer
    | JSLitBool Bool
    | JSLitArray [JSExpr]
    | JSLitObject [(String, JSExpr)]
    | JSLitNull
    | JSLitNothing
    | JSLitUndefined
    deriving (Show, Eq)
