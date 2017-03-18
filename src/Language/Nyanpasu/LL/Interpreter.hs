{- | An interpreter for `AST.Expr`
-}

module Language.Nyanpasu.LL.Interpreter where

import Data.Bits
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Text.Groom (groom)

import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.Error
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Assembly.X86.CodeGen (trueValue, falseValue, boolTag)

type Val = Atom ()

type Env = M.Map Name Val

type Interpreter a
  = StateT Env (Except Error) a

interpret :: Show a => Expr a -> Either Error Val
interpret =
  runExcept . flip evalStateT M.empty . runInterpreter

runInterpreter :: Show a => Expr a -> Interpreter Val
runInterpreter = \case
  Atom (Num _ i) ->
    pure $ Num () i

  Atom (Bool _ b) ->
    pure $ Bool () b

  expr@(PrimOp _ op e) -> case op of
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


  expr@(PrimBinOp _ op_ e1 e2) -> case op_ of
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
          Or -> (||)

  Let _ binder bind e -> do
    r <- runInterpreter bind
    modify (M.insert binder r)
    runInterpreter e

  Idn _ name -> lookupM id name

  expr@(If _ test true false) -> do
    r <- runInterpreter test
    bool1M expr r $ \b ->
      runInterpreter $
        if b
          then true
          else false




num1 :: Show a => Expr a -> (Int32 -> Val) -> Val -> Interpreter Val
num1 expr f = \case
  Num _ i ->
    pure (f i)

  Bool _ b ->
    throwErr . unlines $
      [ "Type error. Expecting a number but got the boolean value " ++ show b
      , "In the expression: "
      , groom expr
      ]

num2 :: Show a => Expr a -> (Int32 -> Int32 -> Val) -> Val -> Val -> Interpreter Val
num2 expr f v1 v2 = case v1 of
  Num _ i ->
    num1 expr (f i) v2

  Bool _ b ->
    throwErr . unlines $
      [ "Type error. Expecting a number but got the boolean value " ++ show b
      , "In the expression: "
      , groom expr
      ]

bool1 :: Show a => Expr a -> (Bool -> Bool) -> Val -> Interpreter Val
bool1 expr f = \case
  Bool _ b ->
    pure (Bool () $ f b)

  Num _ i ->
    throwErr . unlines $
      [ "Type error. Expecting a boolean value but got the number " ++ show i
      , "In the expression: "
      , groom expr
      ]

bool2 :: Show a => Expr a -> (Bool -> Bool -> Bool) -> Val -> Val -> Interpreter Val
bool2 expr f v1 v2 = case v1 of
  Bool _ b ->
    bool1 expr (f b) v2

  Num _ i ->
    throwErr . unlines $
      [ "Type error. Expecting a boolean value but got the number " ++ show i
      , "In the expression: "
      , groom expr
      ]

bool1M :: Show a => Expr a -> Val -> (Bool -> Interpreter Val) -> Interpreter Val
bool1M expr v f = case v of
  Bool _ b ->
    f b

  Num _ i ->
    throwErr . unlines $
      [ "Type error. Expecting a boolean value but got the number " ++ show i
      , "In the expression: "
      , groom expr
      ]


int32ToVal :: Int32 -> Either Error Val
int32ToVal i
  | i .&. boolTag /= 0
  , i == trueValue = pure (Bool () True)

  | i .&. boolTag /= 0
  , i == falseValue = pure (Bool () False)

  | i .&. boolTag /= 0 = throwErr $ "Unexpected value: " ++ show i

  | otherwise = pure (Num () $ shiftR i 1)
