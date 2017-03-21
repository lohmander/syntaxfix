module Syntax where

import qualified Data.Map.Strict as Map


type Name   = String
type Param  = String
type Export = String
type Runs   = String
type FuncId = (String, Int)
type Vars   = Map.Map String Expr


data Module
    = Module Name [Export] [Runs] [Declaration]
    deriving (Show)


data SourceType
    = JS
    | Sunsu
    deriving (Show)


data ImportAssign
    = DefaultAs Name
    | As Name Name
    | Plain Name
    deriving (Show)


data Declaration
    = Func FuncId [Param] [Expr] Vars
    | Const Name Expr
    | Import SourceType Name [ImportAssign]
    deriving (Show)


data Expr
    = Var Name
    | Lit Lit
    | App Expr [Expr]
    | Lambda [Param] Expr
    | Arith Op Expr Expr
    deriving (Show)


data Op
    = Add
    | Subtract
    | Mulitply
    | Divide
    | PowerOf
    | Modulos
    deriving (Show)


data Lit
    = Bool Bool
    | String String
    | Int Int
    | Float Float
    deriving (Show)
