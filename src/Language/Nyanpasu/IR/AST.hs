{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.IR.AST where

import Data.Data
import GHC.Generics
import Control.DeepSeq
import Data.Generics.Uniplate.Data
import Language.Nyanpasu.Types

-------------------
-- Functional IR --
-------------------

-- | An immediate value
--
data Atom a
  = Num a Int32
  | Bool a Bool
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)

-- | A data type for errors
--
data TypeError
  = NotANumber
  | NotABool
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

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
  | Call a Name [Expr a]
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)


-- | A definition of a function or value
--
data Def a
  = Fun a Name [Name] (Expr a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)

-- | The Program type
--   Represents a sequence of expressions
--
data Program a = Program
  { progDefs :: [Def a]
  , progMain :: Expr a
  }
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)


-- | The PrimOp type
--   represents a primitive operation in the language
--
data PrimOp
  = NumOp NumOp
  | BoolOp BoolOp
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | Unary num operation
data NumOp
  = Inc
  | Dec
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | Unary bool operation
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
  | Xor
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-----------------
-- Annotations --
-----------------

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

instance Annotated Expr where
  getAnn :: Data a => Expr a -> a
  getAnn = head . childrenBi
  setAnn :: a -> Expr a -> Expr a
  setAnn ann = \case
    Atom a -> Atom (setAnn ann a)
    PrimOp _ op e -> PrimOp ann op e
    PrimBinOp _ op e1 e2 -> PrimBinOp ann op e1 e2
    Idn _ i -> Idn ann i
    Let _ name bind body -> Let ann name bind body
    If _ test falseBranch trueBranch -> If ann test falseBranch trueBranch
    Call _ fun args -> Call ann fun args

ppAtom :: Atom a -> String
ppAtom = \case
  Bool _ b -> show b
  Num _  n -> show n

-----------------
-- AST Helpers --
-----------------

fun_ :: Name -> [Name] -> Expr () -> Def ()
fun_ = Fun ()

call_ :: String -> [Expr ()] -> Expr ()
call_ = Call ()

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

defNames :: Program a -> [Name]
defNames prog = flip map (progDefs prog) $ \case
  Fun _ name _ _ -> name

