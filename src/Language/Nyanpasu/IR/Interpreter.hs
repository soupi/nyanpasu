{- | An interpreter for `AST.Expr`
-}

module Language.Nyanpasu.IR.Interpreter where

import Data.Bits
import Data.Data
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Language.Nyanpasu.Types
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.AST
import Language.Nyanpasu.IR.CodeGen (trueValue, falseValue, boolTag)

type Env a = M.Map Name (Atom a)

type Interpreter ann a
  = StateT (Env ann) (Except Error) a

interpret :: (Data a, Show a) => Expr a -> Either Error (Atom a)
interpret =
  runExcept . flip evalStateT M.empty . runInterpreter

runInterpreter :: (Data a, Show a) => Expr a -> Interpreter a (Atom a)
runInterpreter expr = case expr of
  Atom (Num ann i) ->
    pure $ Num ann i

  Atom (Bool ann b) ->
    pure $ Bool ann b

  PrimOp ann op e -> case op of
    NumOp op' -> case op' of
      Inc ->
        num1 expr (Num ann . (+1))
          =<< runInterpreter e

      Dec ->
        num1 expr (Num ann . (\x -> x - 1))
          =<< runInterpreter e

    BoolOp op' -> case op' of
      Not ->
        bool1 expr not
          =<< runInterpreter e


  PrimBinOp ann op_ e1 e2 -> case op_ of
    NumBinOp op' ->
      join $ num2 expr op
        <$> runInterpreter e1
        <*> runInterpreter e2
      where
        op = case op' of
          Add       -> Num  ann  .* (+)
          Sub       -> Num  ann  .* (-)
          Mul       -> Num  ann  .* (*)
          Eq        -> Bool ann .* (==)
          NotEq     -> Bool ann .* (/=)
          Less      -> Bool ann .* (<)
          LessEq    -> Bool ann .* (<=)
          Greater   -> Bool ann .* (>)
          GreaterEq -> Bool ann .* (>=)

    BoolBinOp op' ->
      join $ bool2 expr op
        <$> runInterpreter e1
        <*> runInterpreter e2
      where
        op = case op' of
          And -> (&&)
          Or  -> (||)
          Xor -> xor

  Let _ binder (Right' bind) e -> do
    r <- runInterpreter bind
    modify (M.insert binder r)
    runInterpreter e

  ast@(Let _ _ (Left' _) _) -> do
    throwError $ InternalError $ unlines
      [ "Found let with lambda in AstToAnf. Expected lambda lifting to take care of it."
      , groom ast
      ]

  Idn _ name -> lookupM id name

  If _ test true false -> do
    r <- runInterpreter test
    bool1M expr r $ \b ->
      runInterpreter $
        if b
          then true
          else false


num1 :: (Show a, Data a) => Expr a -> (Int32 -> Atom a) -> Atom a -> Interpreter a (Atom a)
num1 expr f v = case v of
  Num _  i -> pure (f i)
  _ -> throwTErr NotANumber (setAnn (getAnn expr) v) expr

num2 :: (Show a, Data a) => Expr a -> (Int32 -> Int32 -> Atom a) -> Atom a -> Atom a -> Interpreter a (Atom a)
num2 expr f v1 v2 = case v1 of
  Num _  i -> num1 expr (f i) v2
  _ -> throwTErr NotANumber (setAnn (getAnn expr) v1) expr

bool1 :: (Show a, Data a) => Expr a -> (Bool -> Bool) -> Atom a -> Interpreter a (Atom a)
bool1 expr f v = case v of
  Bool _ b -> pure (Bool (getAnn expr) $ f b)
  _ -> throwTErr NotABool (setAnn (getAnn expr) v) expr

bool2 :: (Show a, Data a) => Expr a -> (Bool -> Bool -> Bool) -> Atom a -> Atom a -> Interpreter a (Atom a)
bool2 expr f v1 v2 = case v1 of
  Bool _ b -> bool1 expr (f b) v2
  _ -> throwTErr NotABool (setAnn (getAnn expr) v1) expr

bool1M :: (Show a, Data a) => Expr a -> Atom a -> (Bool -> Interpreter a (Atom a)) -> Interpreter a (Atom a)
bool1M expr v f = case v of
  Bool _ b -> f b
  _ -> throwTErr NotABool (setAnn (getAnn expr) v) expr


int32ToVal :: a -> Int32 -> Either Error (Atom a)
int32ToVal ann i
  | i .&. boolTag /= 0
  , i == trueValue = pure (Bool ann True)

  | i .&. boolTag /= 0
  , i == falseValue = pure (Bool ann False)

  | i .&. boolTag /= 0 = throwErr $ "Unexpected value: " ++ show i

  | otherwise = pure (Num ann $ shiftR i 1)
