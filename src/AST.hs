module AST
  ( Module(..)
  , ImportType(..)
  , Decl(..)
  , Expr(..)
  , ArithOp(..)
  , Lit(..)
  ) where


data Module
    = Module String [String] [String] [Decl]
    deriving (Eq, Show)


data ImportType
    = ImportJS
    | ImportSyntaxfix
    deriving (Eq, Show)


data Decl
    = DeclFunc String [String] [Expr] [(String, Expr)]
    | DeclImport ImportType String (Maybe String) [(Maybe String, String)]
    deriving (Eq, Show)


data Expr
    = ExprLit Lit
    | ExprApp Expr [Expr]
    | ExprVar String
    | ExprArith ArithOp Expr Expr
    | ExprPipe Expr Expr
    | ExprLambda [String] Expr
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
    | LitRecord [(String, Expr)]
    | LitNothing
    | LitNull
    | LitUndefied
    deriving (Eq, Show)
