{- | Utilites for code generation. Also includes the AST -> ANF algorithm

-}


module Language.Nyanpasu.IR.Rewrites.ANF.AstToAnf where

import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.CodeGenUtils
import Language.Nyanpasu.IR.ANF
import qualified Language.Nyanpasu.IR.AST as AST

import Control.Monad.State
import Control.Monad.Except

import Data.List (foldl')
import Data.Data (Data)


------------------------
-- Convert AST to ANF --
------------------------

-- | Use this to run the algorithm
runExprToANF :: AST.Expr () -> Except Error (Expr ())
runExprToANF = flip evalStateT (initState []) . exprToANF

-- | A-normalize a program
normalizeProgram :: Data a => AST.Program a -> Except Error (Program a)
normalizeProgram prog = do
  let funNames = map (\name -> (name, (name, Nothing))) (AST.defNames prog)

  (defs, main) <- flip evalStateT (initState funNames) $ do
    funs <- forM (AST.progDefs prog) $ \case
      AST.Fun ann name args body -> do
        modify $ \CodeGenState{..} ->
          let env = zip args [(-2),(-3)..]
          in CodeGenState env 1 cgNamer cgFunctions
        e <- exprToANF body
        pure $ Fun ann (name, Nothing) args e
    modify $ \CodeGenState{..} -> CodeGenState [] 1 cgNamer cgFunctions
    main <- exprToANF (AST.progMain prog)
    pure (funs, main)
  pure (Program defs main)

-- | Algorithm to convert an `AST.Expr a` to `ANF.Expr a`
--   We will also assign permanent addresses for each identifier
exprToANF :: Data a => AST.Expr a -> StateT CodeGenState (Except Error) (Expr a)
exprToANF = \case

  -- Atom is already immediate
  AST.Atom (AST.Num a i) ->
    pure $ Atom (Num a i)

  AST.Atom (AST.Bool a b) ->
    pure $ Atom (Bool a b)

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
            pure (Let (getAnn e1') addr1 e1', Idn (getAnn e1') addr1, True)

    e2' <- exprToANF e2
    (let2, idn2, pop2) <-
        case getAtom e2' of
          Just res -> pure (id, res, False)
          Nothing  -> do
            addr2 <- insertNamer
            pure (Let (getAnn e2') addr2 e2', Idn (getAnn e2') addr2, True)

    sequence_ [ popVar | p <- [pop1, pop2], p ]

    pure $
      let1 $ let2 $
        PrimBinOp a op idn1 idn2

  -- All arguments must be converted to be immediate
  AST.Call a funcName exprs -> do
    results <- forM exprs $ \e -> do
      e' <- exprToANF e
      case getAtom e' of
        Just res -> pure ((id, res), False)
        Nothing  -> do
          addr <- insertNamer
          pure ((Let (getAnn e') addr e', Idn (getAnn e') addr), True)

    sequence_ [ popVar | (_, True) <- results ]

    pure $
      foldl' (flip (.)) id (map (fst . fst) results) $
        Call a (funcName, Nothing) (map (snd . fst) results)

