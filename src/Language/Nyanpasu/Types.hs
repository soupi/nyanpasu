{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, DeriveTraversable, DeriveFoldable #-}

module Language.Nyanpasu.Types where

import qualified Data.Int as Int (Int32)
import Data.Data
import GHC.Generics
import Control.DeepSeq

type Int32 = Int.Int32
type Name = String


data Either' f g a
  = Left'  (f a)
  | Right' (g a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable, Functor, Foldable, Traversable)
