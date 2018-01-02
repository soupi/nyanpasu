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
  , Def(..)
  , Program(..)
  , mapExprDef
  , mapExprProgram
  , mapExprDefs
  , getAnn
  , setAnn
  , getAtom
  , defLabels
  )
where

import Language.Nyanpasu.IR.AST as Export
  ( PrimOp(..)
  , NumOp(..)
  , BoolOp(..)
  , PairOp(..)
  , PrimBinOp(..)
  , NumBinOp(..)
  , BoolBinOp(..)
  )

import Language.Nyanpasu.Types

import Data.Data
import GHC.Generics
import Control.DeepSeq
import Data.Generics.Uniplate.Data

import Language.Nyanpasu.Assembly.X86 (Label)

-----------------------
-- ANF Functional IR --
-----------------------

-- | The Expr type
--   Represents the low level, yet functional IR
--
data Expr a
  = Atom (Atom a)
  | MkPair a (Atom a) (Atom a)
  | PrimOp a PrimOp (Atom a)
  | PrimBinOp a PrimBinOp (Atom a) (Atom a)
  | Let a Address (Expr a) (Expr a)
  | If a (Atom a) (Expr a) (Expr a)
  | Call a Label [Atom a]
  | CCall a Label [Atom a]
  | TailCall a Label Int32 [Atom a]
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


-- | A definition of a function or value
--
data Def a
  = Fun
    a         -- Annotation
    Label     -- Function name/label
    [Name]    -- arguments
    (Expr a)  -- Body
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)

mapExprDef :: (Expr a -> Expr a) -> Def a -> Def a
mapExprDef f = \case
  Fun a name args body ->
    Fun a name args (f body)


-- | The Program type
--   Represents a sequence of expressions
--
data Program a = Program
  { progDefs :: [Def a]
  , progMain :: Expr a
  }
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)

mapExprProgram :: (Expr a -> Expr a) -> Program a -> Program a
mapExprProgram f prog =
  Program
    { progDefs = mapExprDef f <$> progDefs prog
    , progMain = f (progMain prog)
    }

mapExprDefs :: (Expr a -> Expr a) -> Program a -> Program a
mapExprDefs f prog =
  Program
    { progDefs = mapExprDef f <$> progDefs prog
    , progMain = progMain prog
    }

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
    MkPair _ e1 e2 -> MkPair ann e1 e2
    PrimOp _ op e -> PrimOp ann op e
    PrimBinOp _ op e1 e2 -> PrimBinOp ann op e1 e2
    Let _ name bind body -> Let ann name bind body
    If _ test falseBranch trueBranch -> If ann test falseBranch trueBranch
    Call _ fun args -> Call ann fun args
    CCall _ fun args -> CCall ann fun args
    TailCall _ fun callSpace args -> TailCall ann fun callSpace args

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


defLabels :: Program a -> [Label]
defLabels prog = flip map (progDefs prog) $ \case
  Fun _ name _ _ -> name
