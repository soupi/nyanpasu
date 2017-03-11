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

-- | An immediate value
--
data Atom a
  = Num a Int
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor)

-- | The Expr type
--   Represents the low level, yet functional IR
--
data Expr a
  = Atom (Atom a)
  | PrimOp a PrimOp (Expr a)
  | PrimBinOp a PrimBinOp (Expr a) (Expr a)
  | Idn a Name
  | Let a Name (Expr a) (Expr a)
  | If a (Expr a) (Expr a) (Expr a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor)

-- | The PrimOp type
--   represents a primitive operation in the language
--
data PrimOp
  = Inc
  | Dec
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The PrimBinOp type
--   represents a primitive binary operation in the language
--
data PrimBinOp
  = Add
  | Sub
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

---------------------
-- Class Instances --
---------------------

instance Foldable Atom where
  foldMap f = \case
    Num a _ -> f a

instance Traversable Atom where
  traverse f = \case
    Num a i -> flip Num i <$> f a

instance Foldable Expr where
  foldMap f = \case
    Atom a -> foldMap f a
    PrimOp a _ e -> f a <> foldMap f e
    PrimBinOp a _ e1 e2 -> f a <> foldMap f e1 <> foldMap f e2
    Idn a _ -> f a
    Let a _ b e -> f a <> foldMap f b <> foldMap f e
    If a t t' f' -> f a <> foldMap f t <> foldMap f t' <> foldMap f f'

instance Traversable Expr where
  traverse f = \case
    Atom a -> Atom <$> traverse f a
    PrimOp a op e -> PrimOp <$> f a <*> pure op <*> traverse f e
    PrimBinOp a op e1 e2 -> PrimBinOp <$> f a <*> pure op <*> traverse f e1 <*> traverse f e2
    Idn a n -> flip Idn n <$> f a
    Let a n b e -> flip Let n <$> f a <*> traverse f b <*> traverse f e
    If a t t' f' ->
      If <$> f a <*> traverse f t <*> traverse f t' <*> traverse f f'


-----------------
-- Annotations --
-----------------

getAnn :: Expr a -> a
getAnn = \case
  Atom (Num a _) -> a
  PrimOp a _ _ -> a
  PrimBinOp a _ _ _ -> a
  Idn a _ -> a
  Let a _ _ _ -> a
  If  a _ _ _ -> a

setAnn :: a -> Expr a -> Expr a
setAnn ann = \case
  Atom (Num _ e) -> Atom (Num ann e)
  PrimOp _ op e -> PrimOp ann op e
  PrimBinOp _ op e1 e2 -> PrimBinOp ann op e1 e2
  Idn _ i -> Idn ann i
  Let _ n b e -> Let ann n b e
  If _ t f' t' -> If ann t f' t'

-----------------
-- AST Helpers --
-----------------

num :: Int -> Expr ()
num = Atom . Num ()

inc :: Expr () -> Expr ()
inc = PrimOp () Inc

dec :: Expr () -> Expr ()
dec = PrimOp () Dec

idn :: String -> Expr ()
idn = Idn ()

let' :: String -> Expr () -> Expr () -> Expr ()
let' = Let ()

if' :: Expr () -> Expr () -> Expr () -> Expr ()
if' = If ()

add :: Expr () -> Expr () -> Expr ()
add = PrimBinOp () Add

sub :: Expr () -> Expr () -> Expr ()
sub = PrimBinOp () Sub
