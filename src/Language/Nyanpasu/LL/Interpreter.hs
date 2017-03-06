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

  Idn name -> lookupM id name
