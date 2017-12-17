{- | Utilites for code generation. Also includes the AST -> ANF algorithm

-}


module Language.Nyanpasu.IR.CodeGenUtils where

import Language.Nyanpasu.IR.ANF
import Language.Nyanpasu.Types
import Language.Nyanpasu.Error
import Language.Nyanpasu.Assembly.X86

import Data.Data
import Data.Generics.Uniplate.Data
import Data.Monoid
import Control.Monad.State
import Control.Monad.Except


-- | State for code generation
--
data CodeGenState = CodeGenState
  { cgSymbols :: Env
  , cgCounter :: !Address
  , cgNamer   :: !Int32
  , cgFunctions :: [(Name, Label)]
  }
  deriving (Show, Read, Eq, Ord)

initState :: [(Name, Label)] -> CodeGenState
initState = CodeGenState [] 1 0

-- | Env is a list so it can be used as a stack as well
type Env = [(Name, Address)]

-- | State CodeGenState + Except Error
type CodeGen ann a
  = StateT CodeGenState (Except Error) a

-- | Insert a variable into the environment and gen an address for it
insertVar :: String -> CodeGen ann Address
insertVar name = do
  CodeGenState env addr namer funs <- get
  put (CodeGenState ((name, addr) : env) (addr + 1) namer funs)
  pure addr

-- | Get an address for a fresh variable
insertNamer :: CodeGen ann Address
insertNamer = do
  CodeGenState env addr namer funs <- get
  put (CodeGenState env addr (namer + 1) funs)
  let name = "&automatic&" <> show namer
  insertVar name

-- | Pop a variable that is no longer in use
popVar :: CodeGen ann ()
popVar = do
  CodeGenState env addr namer funs <- get
  put (CodeGenState (tail env) (addr - 1) namer funs)


---------
-- ANF --
---------


countVars :: Data a => Expr a -> Address
countVars expr = (+1) . maximum . (0:) $
  [ addr | Let _ addr _ _ <- universe expr ]
