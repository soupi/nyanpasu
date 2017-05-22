{- | Administrative Normal Form (ANF) representation of the functional IR

ANF representation states that all operators to a function must be
immediate values. So we will need to convert complex operations like this:

  inc (inc (num 7))

To something like this:

  let "temp" (inc (num 7)) (inc "temp")

The algorithm to convert from `AST.Expr a` to `ANF.Expr a` can be found in:

Language.Nyanpasu.LL.CodeGenUtils

-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.LL.ANF
  ( module Export
  , Expr(..)
  , Atom(..)
  , Address
  , getAnn
  , setAnn
  , getAtom
  )
where

import Language.Nyanpasu.LL.AST as Export
  ( PrimOp(..)
  , NumOp(..)
  , BoolOp(..)
  , PrimBinOp(..)
  , NumBinOp(..)
  , BoolBinOp(..)
  )

import Language.Nyanpasu.Types

import Data.Monoid
import Data.Data
import GHC.Generics
import Control.DeepSeq

-----------------------
-- ANF Functional IR --
-----------------------

-- | The Expr type
--   Represents the low level, yet functional IR
--
data Expr a
  = Atom (Atom a)
  | PrimOp a PrimOp (Atom a)
  | PrimBinOp a PrimBinOp (Atom a) (Atom a)
  | Let a Address (Expr a) (Expr a)
  | If a (Atom a) (Expr a) (Expr a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor)

-- | An immediate value
--
data Atom a
  = Num a Int32
  | Bool a Bool
  | Idn a Address
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor)

-- | A stack address for a variable
--
type Address = Int32

---------------------
-- Class Instances --
---------------------

instance Foldable Atom where
  foldMap f = \case
    Num a _ -> f a
    Bool a _ -> f a
    Idn a _ -> f a

instance Traversable Atom where
  traverse f = \case
    Num a i -> flip Num i <$> f a
    Bool a n -> flip Bool n <$> f a
    Idn a n -> flip Idn n <$> f a

instance Foldable Expr where
  foldMap f = \case
    Atom a -> foldMap f a
    PrimOp a _ e -> f a <> foldMap f e
    PrimBinOp a _ e1 e2 -> f a <> foldMap f e1 <> foldMap f e2
    Let a _ b e -> f a <> foldMap f b <> foldMap f e
    If a t t' f' -> f a <> foldMap f t <> foldMap f t' <> foldMap f f'

instance Traversable Expr where
  traverse f = \case
    Atom a -> Atom <$> traverse f a
    PrimOp a op e -> PrimOp <$> f a <*> pure op <*> traverse f e
    PrimBinOp a op e1 e2 -> PrimBinOp <$> f a <*> pure op <*> traverse f e1 <*> traverse f e2
    Let a n b e -> flip Let n <$> f a <*> traverse f b <*> traverse f e
    If a t t' f' ->
      If <$> f a <*> traverse f t <*> traverse f t' <*> traverse f f'

-----------------
-- Annotations --
-----------------

getAnn :: Expr a -> a
getAnn = \case
  Atom (Num a _) -> a
  Atom (Bool a _) -> a
  Atom (Idn a _) -> a
  PrimOp a _ _ -> a
  PrimBinOp a _ _ _ -> a
  Let a _ _ _ -> a
  If  a _ _ _ -> a

setAnn :: a -> Expr a -> Expr a
setAnn ann = \case
  Atom (Num _ e) -> Atom (Num ann e)
  Atom (Bool _ e) -> Atom (Bool ann e)
  Atom (Idn _ e) -> Atom (Idn ann e)
  PrimOp _ op e -> PrimOp ann op e
  PrimBinOp _ op e1 e2 -> PrimBinOp ann op e1 e2
  Let _ n b e -> Let ann n b e
  If _ t f' t' -> If ann t f' t'


-----------
-- Utils --
-----------

getAtom :: Expr a -> Maybe (Atom a)
getAtom = \case
  Atom a -> pure a
  _      -> Nothing
