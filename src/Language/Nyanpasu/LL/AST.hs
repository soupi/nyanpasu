{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.LL.AST where

import Language.Nyanpasu.Utils

import Data.Data
import GHC.Generics
import Control.DeepSeq

data Expr
  = Num Int
  | Inc Expr
  | Dec Expr
  | Idn Name
  | Let Name Expr Expr
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

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
