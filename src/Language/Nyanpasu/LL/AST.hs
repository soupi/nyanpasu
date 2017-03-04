{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.LL.AST where

import Data.Data
import GHC.Generics
import Control.DeepSeq

data Expr
  = Num Int
  | Inc Expr
  | Dec Expr
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

data Reg
  = EAX
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

data Arg
  = Const Int
  | Reg Reg
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data Instruction
  = IMov Arg Arg
  | IAdd Arg Arg
  | ISub Arg Arg
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)
