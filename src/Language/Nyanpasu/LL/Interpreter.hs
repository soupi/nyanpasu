{- | An interpreter for `AST.Expr`
-}

module Language.Nyanpasu.LL.Interpreter where

import Data.Bits
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Language.Nyanpasu.Types
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.Assembly.X86.CodeGen (trueValue, falseValue, boolTag)

type Val = Atom ()

type Env = M.Map Name Val

type Interpreter ann a
  = StateT Env (Except (Error ann)) a

interpret :: Show a => Expr a -> Either (Error a) Val
interpret =
  runExcept . flip evalStateT M.empty . runInterpreter

runInterpreter :: Show a => Expr a -> Interpreter a Val
runInterpreter expr = case expr of
  Atom (Num _ i) ->
    pure $ Num () i

  Atom (Bool _ b) ->
    pure $ Bool () b

  TypeError te a ->
    throwTErr te a expr

  PrimOp _ op e -> case op of
    NumOp op' -> case op' of
      Inc ->
        num1 expr (Num () . (+1))
          =<< runInterpreter e

      Dec ->
        num1 expr (Num () . (\x -> x - 1))
          =<< runInterpreter e

    BoolOp op' -> case op' of
      Not ->
        bool1 expr not
          =<< runInterpreter e


  PrimBinOp _ op_ e1 e2 -> case op_ of
    NumBinOp op' ->
      join $ num2 expr op
        <$> runInterpreter e1
        <*> runInterpreter e2
      where
        op = case op' of
          Add       -> Num ()  .* (+)
          Sub       -> Num ()  .* (-)
          Mul       -> Num ()  .* (*)
          Eq        -> Bool () .* (==)
          NotEq     -> Bool () .* (/=)
          Less      -> Bool () .* (<)
          LessEq    -> Bool () .* (<=)
          Greater   -> Bool () .* (>)
          GreaterEq -> Bool () .* (>=)

    BoolBinOp op' ->
      join $ bool2 expr op
        <$> runInterpreter e1
        <*> runInterpreter e2
      where
        op = case op' of
          And -> (&&)
          Or  -> (||)
          Xor -> xor

  Let _ binder bind e -> do
    r <- runInterpreter bind
    modify (M.insert binder r)
    runInterpreter e

  Idn _ name -> lookupM id name

  If _ test true false -> do
    r <- runInterpreter test
    bool1M expr r $ \b ->
      runInterpreter $
        if b
          then true
          else false


num1 :: Expr a -> (Int32 -> Val) -> Val -> Interpreter a Val
num1 expr f v = case v of
  Num _  i -> pure (f i)
  _ -> throwTErr NotANumber (setAnnAtom (getAnn expr) v) expr

num2 :: Expr a -> (Int32 -> Int32 -> Val) -> Val -> Val -> Interpreter a Val
num2 expr f v1 v2 = case v1 of
  Num _  i -> num1 expr (f i) v2
  _ -> throwTErr NotANumber (setAnnAtom (getAnn expr) v1) expr

bool1 :: Expr a -> (Bool -> Bool) -> Val -> Interpreter a Val
bool1 expr f v = case v of
  Bool _ b -> pure (Bool () $ f b)
  _ -> throwTErr NotABool (setAnnAtom (getAnn expr) v) expr

bool2 :: Expr a -> (Bool -> Bool -> Bool) -> Val -> Val -> Interpreter a Val
bool2 expr f v1 v2 = case v1 of
  Bool _ b -> bool1 expr (f b) v2
  _ -> throwTErr NotABool (setAnnAtom (getAnn expr) v1) expr

bool1M :: Expr a -> Val -> (Bool -> Interpreter a Val) -> Interpreter a Val
bool1M expr v f = case v of
  Bool _ b -> f b
  _ -> throwTErr NotABool (setAnnAtom (getAnn expr) v) expr


int32ToVal :: Int32 -> Either (Error ann) Val
int32ToVal i
  | i .&. boolTag /= 0
  , i == trueValue = pure (Bool () True)

  | i .&. boolTag /= 0
  , i == falseValue = pure (Bool () False)

  | i .&. boolTag /= 0 = throwErr $ "Unexpected value: " ++ show i

  | otherwise = pure (Num () $ shiftR i 1)
