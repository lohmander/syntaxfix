module AST
  ( Module(..)
  , ImportType(..)
  , FuncId
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


type FuncId = (String, Int)


data Decl
    = DeclFunc FuncId [String] [Expr] [(String, Expr)]
    | DeclOverloadedFunc String [Decl]
    | DeclImport ImportType String (Maybe String) [(Maybe String, String)]
    | DeclConst String Expr
    deriving (Eq, Show)


data Expr
    = ExprLit Lit
    | ExprApp Expr [Expr]
    | ExprParensProp Expr String
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
