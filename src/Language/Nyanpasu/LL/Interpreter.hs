module Language.Nyanpasu.LL.Interpreter where

import Data.Monoid
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.Error

type Env = M.Map Name Int

type Interpreter a
  = StateT Env (Except Error) a

interpret :: Expr -> Either Error Int
interpret =
  runExcept . flip evalStateT M.empty . runInterpreter

runInterpreter :: Expr -> Interpreter Int
runInterpreter = \case
  Num i ->
    pure i

  Inc e ->
    (+1) <$> runInterpreter e

  Dec e ->
    (\x -> x-1) <$> runInterpreter e

  Let binder bind e -> do
    r <- runInterpreter bind
    modify (M.insert binder r)
    runInterpreter e

  Idn name -> do
    mResult <- M.lookup name <$> get
    maybe
      (throwErr $ "Undeclared identifier '" <> name <> "'")
      pure
      mResult

