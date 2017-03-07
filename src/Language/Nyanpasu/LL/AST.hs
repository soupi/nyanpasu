{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.LL.AST where

import Language.Nyanpasu.Utils

import Data.Data
import GHC.Generics
import Control.DeepSeq

data Expr a
  = Num a Int
  | PrimOp a (PrimOp a)
  | Idn a Name
  | Let a Name (Expr a) (Expr a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor)

data Reg
  = EAX
  | ESP
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

data Arg
  = Const Int
  | Reg Reg
  | RegOffset Reg Int
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data Instruction
  = IMov Arg Arg
  | IAdd Arg Arg
  | ISub Arg Arg
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data PrimOp a
  = Inc (Expr a)
  | Dec (Expr a)
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData, Functor)

getAnn :: Expr a -> a
getAnn = \case
  Num a _ -> a
  PrimOp a _ -> a
  Idn a _ -> a
  Let a _ _ _ -> a

setAnn :: a -> Expr a -> Expr a
setAnn ann = \case
  Num _ e -> Num ann e
  PrimOp _ op -> PrimOp ann op
  Idn _ i -> Idn ann i
  Let _ n b e -> Let ann n b e

--------------------
-- No annotations --
--------------------

num :: Int -> Expr ()
num = Num ()

inc :: Expr () -> Expr ()
inc = PrimOp () . Inc

dec :: Expr () -> Expr ()
dec = PrimOp () . Dec

idn :: String -> Expr ()
idn = Idn ()

let' :: String -> Expr () -> Expr () -> Expr ()
let' = Let ()
