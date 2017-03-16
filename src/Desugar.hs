{-# LANGUAGE MultiParamTypeClasses #-}

module Desugar (desugar) where

import           AST
import           Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Maybe


type Env = Map.Map String Decl


desugar :: Module -> Either () Module
desugar module' = Right $ evalState (dModule module') Map.empty


lookupFunc :: String -> State Env (Maybe Decl)
lookupFunc name = do
    env <- get
    return $ Map.lookup name env


overloadFunc :: Maybe Decl -> Decl -> State Env Decl
overloadFunc ofn fn = case (ofn, fn) of
    (Nothing, func@(DeclFunc name _ _ _ _)) -> do
        env <- get
        put $ Map.insert name func env
        return func

    (Just ofn'@(DeclFunc name _ _ _ _), _) -> do
        env <- get
        put $ Map.insert
            name
            (DeclOverloadedFunc name [ofn', fn])
            env
        return ofn'

    (Just ofn'@(DeclOverloadedFunc name fns), _) -> do
        env <- get
        put $ Map.insert
            name
            (DeclOverloadedFunc name $ fns ++ [fn])
            env
        return ofn'


dModule :: Module -> State Env Module
dModule (Module name exports runs decls) = do
    decls' <- dDecls decls
    return $ Module name exports runs decls'


dDecls :: [Decl] -> State Env [Decl]
dDecls decls = do
    mapM dDecl decls
    env <- get
    return $ map snd $ Map.toList env


dDecl :: Decl -> State Env (Maybe Decl)
dDecl decl = case decl of
    func@(DeclFunc name _ _ _ _) -> do
        env   <- get
        func' <- lookupFunc name
        ofn   <- overloadFunc func' func
        return $ Just ofn

    decl ->
        return $ Just decl
