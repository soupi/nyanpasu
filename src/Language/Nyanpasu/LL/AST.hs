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
  = Num a Int32
  | Bool a Bool
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
  = NumOp NumOp
  | BoolOp BoolOp
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data NumOp
  = Inc
  | Dec
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

data BoolOp
  = Not
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)


-- | The PrimBinOp type
--   represents a primitive binary operation in the language
--
data PrimBinOp
  = NumBinOp NumBinOp
  | BoolBinOp BoolBinOp
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | Operations on two numbers
data NumBinOp
  = Add
  | Sub
  | Mul
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | Operations on two booleans
data BoolBinOp
  = And
  | Or
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

---------------------
-- Class Instances --
---------------------

instance Foldable Atom where
  foldMap f = \case
    Num a _ -> f a
    Bool a _ -> f a

instance Traversable Atom where
  traverse f = \case
    Num a i -> flip Num i <$> f a
    Bool a i -> flip Bool i <$> f a

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
  Atom (Bool a _) -> a
  PrimOp a _ _ -> a
  PrimBinOp a _ _ _ -> a
  Idn a _ -> a
  Let a _ _ _ -> a
  If  a _ _ _ -> a

setAnn :: a -> Expr a -> Expr a
setAnn ann = \case
  Atom (Num _ e) -> Atom (Num ann e)
  Atom (Bool _ e) -> Atom (Bool ann e)
  PrimOp _ op e -> PrimOp ann op e
  PrimBinOp _ op e1 e2 -> PrimBinOp ann op e1 e2
  Idn _ i -> Idn ann i
  Let _ n b e -> Let ann n b e
  If _ t f' t' -> If ann t f' t'

-----------------
-- AST Helpers --
-----------------

let_ :: String -> Expr () -> Expr () -> Expr ()
let_ = Let ()

if_ :: Expr () -> Expr () -> Expr () -> Expr ()
if_ = If ()

idn_ :: String -> Expr ()
idn_ = Idn ()

num_ :: Int32 -> Expr ()
num_ = Atom . Num ()

inc_ :: Expr () -> Expr ()
inc_ = PrimOp () (NumOp Inc)

dec_ :: Expr () -> Expr ()
dec_ = PrimOp () (NumOp Dec)

add_ :: Expr () -> Expr () -> Expr ()
add_ = PrimBinOp () (NumBinOp Add)

sub_ :: Expr () -> Expr () -> Expr ()
sub_ = PrimBinOp () (NumBinOp Sub)

mul_ :: Expr () -> Expr () -> Expr ()
mul_ = PrimBinOp () (NumBinOp Mul)

true_ :: Expr ()
true_ = Atom $ Bool () True

false_ :: Expr ()
false_ = Atom $ Bool () False

not_ :: Expr () -> Expr ()
not_ = PrimOp () (BoolOp Not)

and_ :: Expr () -> Expr () -> Expr ()
and_ = PrimBinOp () (BoolBinOp And)

or_ :: Expr () -> Expr () -> Expr ()
or_ = PrimBinOp () (BoolBinOp Or)

eq_ :: Expr () -> Expr () -> Expr ()
eq_ = PrimBinOp () (NumBinOp Eq)

notEq_ :: Expr () -> Expr () -> Expr ()
notEq_ = PrimBinOp () (NumBinOp NotEq)

less_ :: Expr () -> Expr () -> Expr ()
less_ = PrimBinOp () (NumBinOp Less)

lessEq_ :: Expr () -> Expr () -> Expr ()
lessEq_ = PrimBinOp () (NumBinOp LessEq)

greater_ :: Expr () -> Expr () -> Expr ()
greater_ = PrimBinOp () (NumBinOp Greater)

greaterEq_ :: Expr () -> Expr () -> Expr ()
greaterEq_ = PrimBinOp () (NumBinOp GreaterEq)
