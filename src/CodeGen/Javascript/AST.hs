module CodeGen.Javascript.AST where


data JSModule
    = JSModule [JSDecl]
    deriving (Show, Eq)


data JSDecl
    = JSDeclFunc String [String] [JSState]
    | JSDeclExport String
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
    deriving (Show, Eq)
