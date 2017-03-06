module AST
  ( Module(..)
  , Decl(..)
  , Expr(..)
  , ArithOp(..)
  , Lit(..)
  ) where


data Module
    = Module String [String] [Decl]
    deriving (Eq, Show)


data Decl
    = DeclFunc String [String] [Expr] [(String, Expr)]
    deriving (Eq, Show)


data Expr
    = ExprLit Lit
    | ExprApp Expr [Expr]
    | ExprVar String
    | ExprArith ArithOp Expr Expr
    | ExprPipe Expr Expr
    deriving (Eq, Show)


data ArithOp
    = ArithOpAdd
    | ArithOpSubtract
    | ArithOpMultiply
    | ArithOpDivide
    | ArithOpModulus
    | ArithOpPow
    deriving (Eq, Show)


data Lit
    = LitString String
    | LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitList [Expr]
    deriving (Eq, Show)
