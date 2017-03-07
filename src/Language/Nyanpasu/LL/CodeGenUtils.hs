module Language.Nyanpasu.LL.CodeGenUtils where

import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error

import Control.Monad.State
import Control.Monad.Except

data CodeGenState = CodeGenState
  { cgSymbols :: Env
  , cgCounter :: !Address
  }
  deriving (Show, Read, Eq, Ord)

initState :: CodeGenState
initState = CodeGenState [] 1

type Address = Int

type Env = [(Name, Address)]

type CodeGen a
  = StateT CodeGenState (Except Error) a

insertVar :: String -> CodeGen Address
insertVar name = do
  CodeGenState env addr <- get
  put (CodeGenState ((name, addr) : env) (addr + 1))
  pure addr

popVar :: CodeGen ()
popVar = do
  CodeGenState env addr <- get
  put (CodeGenState (tail env) (addr - 1))

assignLabels :: Expr () -> Expr Int
assignLabels = flip evalState 0 . traverse go
  where
    go () = do
      i <- get
      put (i + 1)
      pure i
