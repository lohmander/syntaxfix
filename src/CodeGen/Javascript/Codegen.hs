{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen.Javascript.Codegen
    ( codegen ) where

import           Data.Map.Strict as Map


type Name = String


data CodegenState
    = CodegenState
    { blocks  :: Map.Map BlockId Block
    , current :: BlockId
    }
    deriving (Show)


data BlockId
    = Id Name
    | FuncId Name Int
    deriving (Eq, Show)


data Block
    = Func Name
    deriving (Show)

codegen :: ()
codegen = undefined
