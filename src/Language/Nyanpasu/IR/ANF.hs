{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{- | Administrative Normal Form (ANF) representation of the functional IR

ANF representation states that all operators to a function must be
immediate values. So we will need to convert complex operations like this:

  inc (inc (num 7))

To something like this:

  let "temp" (inc (num 7)) (inc "temp")

The algorithm to convert from `AST.Expr a` to `ANF.Expr a` can be found in:

Language.Nyanpasu.IR.CodeGenUtils

-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.IR.ANF
  ( module Export
  , Expr(..)
  , Atom(..)
  , Address
  , getAnn
  , setAnn
  , getAtom
  )
where

import Language.Nyanpasu.IR.AST as Export
  ( PrimOp(..)
  , NumOp(..)
  , BoolOp(..)
  , PrimBinOp(..)
  , NumBinOp(..)
  , BoolBinOp(..)
  )

import Language.Nyanpasu.Types

import Data.Data
import GHC.Generics
import Control.DeepSeq
import Data.Generics.Uniplate.Data

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
  | Call a (Atom a) [Atom a]
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)

-- | An immediate value
--
data Atom a
  = Num a Int32
  | Bool a Bool
  | Idn a Address
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)

-- | A stack address for a variable
--
type Address = Int32

---------------------
-- Class Instances --
---------------------



class Annotated f where
  getAnn :: Data a => f a -> a
  setAnn :: a -> f a -> f a

instance Annotated Atom where
  getAnn :: Data a => Atom a -> a
  getAnn = head . childrenBi
  setAnn :: b -> Atom a -> Atom b
  setAnn newAnn = \case
    Num  _ n -> Num  newAnn n
    Bool _ b -> Bool newAnn b
    Idn _ i -> Idn newAnn i

instance Annotated Expr where
  getAnn :: Data a => Expr a -> a
  getAnn = head . childrenBi
  setAnn :: a -> Expr a -> Expr a
  setAnn ann = \case
    Atom a -> Atom (setAnn ann a)
    PrimOp _ op e -> PrimOp ann op e
    PrimBinOp _ op e1 e2 -> PrimBinOp ann op e1 e2
    Let _ name bind body -> Let ann name bind body
    If _ test falseBranch trueBranch -> If ann test falseBranch trueBranch
    --Call _ fun args -> Call ann fun args

-----------------
-- Annotations --
-----------------


-----------
-- Utils --
-----------

getAtom :: Expr a -> Maybe (Atom a)
getAtom = \case
  Atom a -> pure a
  _      -> Nothing
