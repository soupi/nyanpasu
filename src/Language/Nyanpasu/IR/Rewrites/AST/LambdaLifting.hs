{- | Lambda lifting

finds lambda expressions defined inside an expression and brings them out to the top level

-}


module Language.Nyanpasu.IR.Rewrites.AST.LambdaLifting
  ( lambdaLifting
  , annotateFreeVars
  )
where

import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.Types
import Language.Nyanpasu.IR.AST
import Data.Generics.Uniplate.Data
import Data.Data
import Data.List
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

lambdaLifting :: (Show a, Data a, MonadError Error m) => Program a -> m (Program a)
lambdaLifting prog = fmap (traceL "lambda lifting") $ flip evalStateT 0 $ do
  defs <- mapM onDef (progDefs prog)
  mains <- onExpr "main" (annotateFreeVars $ progMain prog)
  pure $
    prog
      { progDefs =
        concat defs ++ fst mains
      , progMain = snd mains
      }

onDef
  :: Show a => Data a => MonadState Int m => MonadError Error m
  => Def a -> m [Def a]
onDef (Fun lbl (Lam a args expr)) = do
  (defs, expr') <- onExpr lbl (annotateFreeVars expr)
  pure $ Fun lbl (Lam a args expr') : defs

onExpr
  :: Show a => Data a => MonadState Int m => MonadError Error m
  => String -> Expr (a, FreeVars a) -> m ([Def a], Expr a)
onExpr lbl currExpr = do
  ref <- get
  let
    saturated = saturateLamAndCalls currExpr

    (defs, m) = flip runState ref
      ( sequence
        [ get >>= \n -> modify (+1)
          *> pure (Fun (makeName lbl (fst ann) name (fst lann) n) (Lam (fst lann) args (stripFreeVarsAnn body)))
        | Let ann name (Left' (Lam lann args body)) _ <- universe currExpr
        ]
      )

  expr' <-
    flip transformM currExpr $ \case
      Let ann name (Left' (Lam lann _ _)) inbody -> do
        n <- get
        modify (+1)
        let
          lname = makeName lbl ann name lann n
        pure $ Let ann name (Right' (Idn lann lname)) inbody
      e -> pure e
  m' <- get
  when (m /= m') $ do
    throwErr $ "wrong state number in lambdalifting on expr: " ++ groom currExpr

  undefined -- pure (defs, expr')

makeName :: Show a => String -> a -> String -> a -> Int -> String
makeName lbl ann name lann n =
  intercalate "$"
    [ lbl
    , show ann
    , name
    , show lann
    , show n
    ]

saturateLamAndCalls :: Expr (a, FreeVars a) -> Expr (a, FreeVars a)
saturateLamAndCalls e = 

type FreeVars a = M.Map Name a

stripFreeVarsAnn :: Expr (a, FreeVars a) -> Expr a
stripFreeVarsAnn = fmap fst

annotateFreeVars :: Data a => Expr a -> Expr (a, FreeVars a)
annotateFreeVars expr = case expr of
  Atom{} -> fmap (, mempty) expr

  PrimOp ann primOp e ->
    let e' = annotateFreeVars e
    in PrimOp (ann, snd $ getAnn e') primOp e'

  PrimBinOp ann primBinOp e1 e2 ->
    let
      e1' = annotateFreeVars e1
      e2' = annotateFreeVars e2
    in
      PrimBinOp (ann, snd (getAnn e1') `M.union` snd (getAnn e2')) primBinOp e1' e2'

  Idn ann name ->
    fmap (, M.singleton name ann) expr

  MkPair ann e1 e2 ->
    let
      e1' = annotateFreeVars e1
      e2' = annotateFreeVars e2
    in
      MkPair (ann, snd (getAnn e1') `M.union` snd (getAnn e2')) e1' e2'

  Let ann name (Left' (Lam lamann args lambody)) body ->
    let
      lambody' = annotateFreeVars lambody
      body' = annotateFreeVars body
      lamfreevars = foldr M.delete (snd (getAnn lambody')) (name : args)
      freevars = name `M.delete` (lamfreevars `M.union` snd (getAnn body'))
    in
      Let (ann, freevars) name (Left' (Lam (lamann, lamfreevars) args lambody')) body'

  Let ann name (Right' binder) body ->
    let
      binder' = annotateFreeVars binder
      body' = annotateFreeVars body
      binderfreevars =  name `M.delete` snd (getAnn binder')
      freevars = name `M.delete` (binderfreevars `M.union` snd (getAnn body'))
    in
      Let (ann, freevars) name (Right' (setAnn (fst $ getAnn binder', binderfreevars) binder')) body'

  If ann e1 e2 e3 ->
    let
      e1' = annotateFreeVars e1
      e2' = annotateFreeVars e2
      e3' = annotateFreeVars e3
    in
      If
        (ann, M.unions $ fmap (snd . getAnn) [e1', e2', e3'])
        e1'
        e2'
        e3'
      
  Call ann name es ->
    let
      (anns, es') = unzip $ map ((snd . getAnn &&& id) . annotateFreeVars) es
    in
      Call (ann, M.singleton name ann `M.union` M.unions anns) name es'

  CCall ann name es ->
    let
      (anns, es') = unzip $ map ((snd . getAnn &&& id) . annotateFreeVars) es
    in
      CCall (ann, M.singleton name ann `M.union` M.unions anns) name es'

