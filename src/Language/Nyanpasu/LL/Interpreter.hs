{- | An interpreter for `AST.Expr`
-}

module Language.Nyanpasu.LL.Interpreter where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.Error
import Language.Nyanpasu.Utils

type Env = M.Map Name Int

type Interpreter a
  = StateT Env (Except Error) a

interpret :: Expr a -> Either Error Int
interpret =
  runExcept . flip evalStateT M.empty . runInterpreter

runInterpreter :: Expr a -> Interpreter Int
runInterpreter = \case
  Atom (Num _ i) ->
    pure i

  PrimOp _ op e -> case op of
    Inc ->
      (+1) <$> runInterpreter e

    Dec ->
      (\x -> x-1) <$> runInterpreter e

  PrimBinOp _ op e1 e2 -> case op of
    Add ->
      (+) <$> runInterpreter e1 <*> runInterpreter e2

    Sub ->
      (-) <$> runInterpreter e1 <*> runInterpreter e2

  Let _ binder bind e -> do
    r <- runInterpreter bind
    modify (M.insert binder r)
    runInterpreter e

  Idn _ name -> lookupM id name

  If _ test true false -> do
    r <- runInterpreter test
    runInterpreter $
      if r /= 0
        then true
        else false
