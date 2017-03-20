module CodeGen.Javascript.Syntax where


type Name = String
type Param = String


data Module
    = Module [Statement]
    deriving (Show)


data Statement
    = Expr Expr
    | Func Name [Param] [Statement]
    | Return Expr
    | Export Name
    | If [(Expr, [Statement])]
    deriving (Show)


data Comp
    = Eq
    deriving (Show)


data Expr
    = Lit Lit
    | Var Name
    | App Expr [Expr]
    | Lambda [Param] [Statement]
    | Comp Comp Expr Expr
    deriving (Show)


data Lit
    = JSString String
    | JSBool Bool
    | JSNumber Double
    deriving (Show)
