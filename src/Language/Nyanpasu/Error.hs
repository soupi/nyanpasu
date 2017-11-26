{- | Error data type
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Error where

import Data.Monoid
import Data.Data
import GHC.Generics
import Control.DeepSeq
import Control.Monad.Except
import Text.Groom (groom)

import Language.Nyanpasu.IR.AST (Atom(..), TypeError(..), Expr(..), ppAtom)

data Error a
  = Error String
  | TError TypeError (Atom a) (Expr a)
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

throwErr :: MonadError (Error ann) m => String -> m a
throwErr = throwError . Error

throwTErr :: MonadError (Error ann) m => TypeError -> Atom ann -> Expr ann -> m a
throwTErr te a e = throwError $ TError te a e

displayError :: Show ann => Error ann -> String
displayError = \case
  Error s ->
    "Error: " <> s

  TError NotANumber a e ->
    unlines
      [ "Type Error: Expecting a number, but got: " <> ppAtom a
      , "In Expression: "
      , groom e
      ]

  TError NotABool a e ->
    unlines
      [ "Type Error: Expecting a boolean, but got: " <> ppAtom a
      , "In Expression: "
      , groom e
      ]
