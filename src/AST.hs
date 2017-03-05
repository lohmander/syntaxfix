module AST
  ( Module(..)
  , Decl(..)
  , Expr(..)
  , Lit(..)
  ) where


data Module
    = Module String [String] [Decl]
    deriving (Eq, Show)


data Decl
    = DeclFunc String [String] [Expr]
    deriving (Eq, Show)


data Expr
    = ExprLit Lit
    | ExprApp String [Expr]
    | ExprVar String
    deriving (Eq, Show)


data Lit
    = LitString String
    | LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitList [Expr]
    deriving (Eq, Show)
