{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.LL.AST where

import Language.Nyanpasu.Utils

import Data.Monoid
import Data.Data
import GHC.Generics
import Control.DeepSeq

-------------------
-- Functional IR --
-------------------

-- | The Expr type
--   Represents the low level, yet functional IR
data Expr a
  = Num a Int
  | PrimOp a (PrimOp a)
  | Idn a Name
  | Let a Name (Expr a) (Expr a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor)

-- | The PrimOp type
--   represents a primitive operation in the language
data PrimOp a
  = Inc (Expr a)
  | Dec (Expr a)
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData, Functor)

--------------
-- Assembly --
--------------

-- | The Instruction type
--   represents an x86 assembly instruction
data Instruction
  = IMov Arg Arg
  | IAdd Arg Arg
  | ISub Arg Arg
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Arg type
--   represents an x86 assembly argument to an instruction
data Arg
  = Const Int
  | Reg Reg
  | RegOffset Reg Int
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Reg type
--   represents an x86 assembly register
data Reg
  = EAX
  | ESP
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

---------------------
-- Class Instances --
---------------------

instance Foldable PrimOp where
  foldMap f = \case
    Inc e -> foldMap f e
    Dec e -> foldMap f e

instance Traversable PrimOp where
  traverse f = \case
    Inc e -> Inc <$> traverse f e
    Dec e -> Dec <$> traverse f e

instance Foldable Expr where
  foldMap f = \case
    Num a _ -> f a
    PrimOp a op -> f a <> foldMap f op
    Idn a _ -> f a
    Let a _ b e -> f a <> foldMap f b <> foldMap f e

instance Traversable Expr where
  traverse f = \case
    Num a i -> flip Num i <$> f a
    PrimOp a op -> PrimOp <$> f a <*> traverse f op
    Idn a n -> flip Idn n <$> f a
    Let a n b e -> flip Let n <$> f a <*> traverse f b <*> traverse f e

-----------------
-- Annotations --
-----------------

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
-- No Annotations --
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
