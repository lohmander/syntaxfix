module CodeGen.Javascript.AST where


data JSModule
    = JSModule [JSDecl]
    deriving (Show, Eq)


data JSDecl
    = JSDeclFunc String [String] [JSState]
    deriving (Show, Eq)


data JSState
    = JSStateReturn JSExpr
    | JSStateLoose JSExpr -- REMOVE THIS
    deriving (Show, Eq)


data JSExpr
    = JSExprLit JSLit
    | JSExprApp String [JSExpr]
    | JSExprVar String
    deriving (Show, Eq)


data JSLit
    = JSLitString String
    | JSLitFloat Float
    | JSLitArray [JSExpr]
    deriving (Show, Eq)
