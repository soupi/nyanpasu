{- | Utilites for code generation. Also includes the AST -> ANF algorithm

-}


module Language.Nyanpasu.LL.CodeGenUtils where

import Language.Nyanpasu.LL.ANF
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import qualified Language.Nyanpasu.LL.AST as AST

import Data.Monoid
import Control.Monad.State
import Control.Monad.Except

-- | State for code generation
--
data CodeGenState = CodeGenState
  { cgSymbols :: Env
  , cgCounter :: !Address
  , cgNamer   :: !Int
  }
  deriving (Show, Read, Eq, Ord)

initState :: CodeGenState
initState = CodeGenState [] 1 0

-- | Env is a list so it can be used as a stack as well
type Env = [(Name, Address)]

-- | State CodeGenState + Except Error
type CodeGen a
  = StateT CodeGenState (Except Error) a

-- | Insert a variable into the environment and gen an address for it
insertVar :: String -> CodeGen Address
insertVar name = do
  CodeGenState env addr namer <- get
  put (CodeGenState ((name, addr) : env) (addr + 1) namer)
  pure addr

-- | Get an address for a fresh variable
insertNamer :: CodeGen Address
insertNamer = do
  CodeGenState env addr namer <- get
  put (CodeGenState env addr (namer + 1))
  let name = "&automatic&" <> show namer
  insertVar name

-- | Pop a variable that is no longer in use
popVar :: CodeGen ()
popVar = do
  CodeGenState env addr namer <- get
  put (CodeGenState (tail env) (addr - 1) namer)

-- | Assign a unique label to all sub-expressions
assignLabels :: Expr () -> Expr Int
assignLabels = flip evalState 0 . traverse go
  where
    go () = do
      i <- get
      put (i + 1)
      pure i

------------------------
-- Convert AST to ANF --
------------------------

-- | Use this to run the algorithm
runExprToANF :: AST.Expr () -> Either Error (Expr Int)
runExprToANF astExpr = fmap assignLabels . runExcept . flip evalStateT initState $ exprToANF astExpr

-- | Algorithm to convert an `AST.Expr a` to `ANF.Expr a`
--   We will also assign permanent addresses for each identifier
exprToANF :: AST.Expr a -> StateT CodeGenState (Except Error) (Expr a)
exprToANF = \case

  -- Atom is already immediate
  AST.Atom (AST.Num a i) ->
    pure $ Atom (Num a i)

  -- convert the argument to anf and add a let if the argument is not immediate
  AST.PrimOp a op e -> do
    e' <- exprToANF e

    case getAtom e' of
      Nothing -> do
        addr <- insertNamer
        popVar
        pure $ Let (getAnn e') addr e' $
          PrimOp a op (Idn (getAnn e') addr)

      Just atom ->
        pure $ PrimOp a op atom

  -- assign an address the a name
  AST.Idn a name ->
    Atom . Idn a
      <$> llookupM cgSymbols name

  -- assign an address the the name
  AST.Let a name binder body -> do
    bind <- exprToANF binder
    addr <- insertVar name
    expr <- exprToANF body
    popVar
    pure $ Let a addr bind expr

  -- `test` must be converted to be immediate
  AST.If a test true false -> do
    test'  <- exprToANF test
    iff <- case getAtom test' of
      Nothing -> do
        addr <- insertNamer
        popVar
        pure $ \true' false' ->
          Let (getAnn test') addr test' $
            If a (Idn (getAnn test') addr) true' false'
      Just t' ->
        pure $ If a t'
    true'  <- exprToANF true
    false' <- exprToANF false
    pure $ iff true' false'
   
  -- Both arguments must be converted to be immediate
  AST.PrimBinOp a op e1 e2 -> do
    e1' <- exprToANF e1
    (let1, idn1, pop1) <-
        case getAtom e1' of
          Just res -> pure (id, res, False)
          Nothing  -> do
            addr1 <- insertNamer
            pure (Let (getAnn e1') addr1 e1', (Idn (getAnn e1') addr1), True)

    e2' <- exprToANF e2
    (let2, idn2, pop2) <-
        case getAtom e2' of
          Just res -> pure (id, res, False)
          Nothing  -> do
            addr2 <- insertNamer
            pure (Let (getAnn e2') addr2 e2', (Idn (getAnn e2') addr2), True)

    sequence_ [ popVar | p <- [pop1, pop2], p ]

    pure $
      let1 $ let2 $
        PrimBinOp a op idn1 idn2

