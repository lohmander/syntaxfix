{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module CodeGen.Javascript.Transformer
    ( transform
    , codegen
    ) where

import           Control.Monad.State
import           Data.Map.Strict           ((!))
import qualified Data.Map.Strict           as Map

import qualified AST                       as Sus
import qualified CodeGen.Javascript.Syntax as JS


-------------------------------------------------
-- Codegen Monad
-------------------------------------------------

type Name = String
type BlockId = (String, Int)


data CodegenState
    = CodegenState
    { runs         :: [JS.Statement]
    , exports      :: [JS.Statement]
    , currentBlock :: BlockId
    , blocks       :: Map.Map BlockId BlockState
    }
    deriving (Show)


data BlockState
    = Const { name :: Name, value :: JS.Expr }
    | Func  { name :: Name, implementations :: Map.Map Int ([Name], [JS.Statement]) }
    deriving (Show)


newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)


emptyBlockId :: BlockId
emptyBlockId = ("", 0)


blockId :: String -> Int -> BlockId
blockId n x = (n, x)


blockId_ :: String -> BlockId
blockId_ = (flip blockId) 0


emptyCodegen :: CodegenState
emptyCodegen = CodegenState [] [] emptyBlockId Map.empty


execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen


--------------------------------------------------
-- Utils
--------------------------------------------------


mapToList :: Map.Map a b -> [b]
mapToList = map snd . Map.toList


addExport :: Name -> Codegen ()
addExport x = modify $ \s -> s { exports = (exports s) ++ [JS.Export x] }


addRun :: Name -> Codegen ()
addRun x = modify $ \s -> s
    { runs = (runs s) ++ [JS.Expr $ JS.App (JS.Var x) [] ] }



addFunc :: Sus.FuncId -> [Name] -> Codegen ()
addFunc fid params = modify $ \s -> s
    { blocks = Map.insert bid (func s) (blocks s)
    , currentBlock = fid
    }
  where
    name = fst fid
    pc   = snd fid
    bid  = blockId_ name

    func s = case Map.lookup bid (blocks s) of
        Nothing ->
            Func "asd" $ Map.singleton pc (params, [])

        Just (Func _ im) ->
            Func "qwer" $ Map.insert pc (params, []) im

        _ ->
            Func "lj" Map.empty


getId :: Codegen BlockId
getId = gets currentBlock


getCurrentBlock :: Codegen BlockState
getCurrentBlock = do
    current <- getId
    blocks  <- gets blocks
    return $ maybe (Func "" Map.empty) id $ Map.lookup current blocks


addStatement :: JS.Statement -> Codegen ()
addStatement st = do
    (name', pc)   <- getId
    (Func _ impl) <- getCurrentBlock

    let (par, sts) = maybe ([], []) id $ Map.lookup pc impl
    let fid        = blockId_ name'
    let func       = Func name' $ Map.insert pc (par, sts ++ [st]) impl

    modify $ \s -> s { blocks = Map.insert fid func (blocks s) }


funcImpl :: (Int, ([Name], [JS.Statement])) -> (JS.Expr, [JS.Statement])
funcImpl (pc, (params, sts)) =
    ( JS.Comp JS.Eq (JS.Var "arguments.length") (JS.Lit $ JS.JSNumber $ fromIntegral pc)
    , [ JS.Return $ JS.App
          (JS.Lambda params sts') $
          map (\x -> JS.Var $ "arguments[" ++ (show x) ++ "]") [0..pc-1]
      ]
    )
  where
    i              = init sts
    (JS.Expr expr) = last sts
    sts'           = i ++ [ JS.Return expr ]


blockToJs :: BlockState -> JS.Statement
blockToJs block = case block of
    Func name' implementations' ->
        JS.Func name' [] [ JS.If $ map funcImpl $ Map.toList implementations' ]


--------------------------------------------------
-- Generation
--------------------------------------------------


transform :: Sus.Module -> JS.Module
transform = codegen


codegen :: Sus.Module -> JS.Module
codegen mod' =
    codegenStateToModule $ execCodegen $ gModule mod'
  where
    codegenStateToModule (CodegenState { blocks, runs }) =
        JS.Module $ (map blockToJs $ mapToList blocks) ++ runs


gModule :: Sus.Module -> Codegen ()
gModule (Sus.Module _ exports' runs' decls) = do
    mapM_ gDecl decls
    mapM_ addRun runs'
    mapM_ addExport exports'


gDecl :: Sus.Decl -> Codegen ()
gDecl decl = case decl of
    Sus.DeclFunc fnId params expr vars -> do
        addFunc fnId params
        mapM_ gExpr expr

    _ ->
        return ()


gExpr :: Sus.Expr -> Codegen ()
gExpr expr = case expr of
    Sus.ExprLit lit ->
        addStatement $ JS.Expr $ JS.Lit $ gLit lit

    Sus.ExprVar var ->
        addStatement $ JS.Expr $ JS.Var var


gLit :: Sus.Lit -> JS.Lit
gLit lit = case lit of
    Sus.LitString str ->
        JS.JSString str

    Sus.LitBool bool ->
        JS.JSBool bool
