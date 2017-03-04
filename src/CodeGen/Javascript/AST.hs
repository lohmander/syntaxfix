module CodeGen.Javascript.AST where


data JSModule
    = JSModule [JSDecl]
    deriving (Show, Eq)


data JSDecl
    = JSDeclFunc String [String] [JSState]
    deriving (Show, Eq)


data JSState
    = JSStateReturn JSExpr
    deriving (Show, Eq)


data JSExpr
    = JSExprLit JSLit
    deriving (Show, Eq)


data JSLit
    = JSLitString String
    | JSLitFloat Float
    deriving (Show, Eq)
