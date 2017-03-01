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
    = DeclFunc { name :: String, body :: Expr }
    deriving (Eq, Show)


data Expr
    = ExprLit Lit
    deriving (Eq, Show)


data Lit
    = LitString String
    | LitInt Int
    | LitFloat Float
    | LitBool Bool
    deriving (Eq, Show)
