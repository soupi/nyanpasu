{- | Error data type
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Error where

import Data.Monoid
import Data.Data
import GHC.Generics
import Control.DeepSeq
import Control.Monad.Except

newtype Error
  = Error String
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

throwErr :: MonadError Error m => String -> m a
throwErr = throwError . Error

displayError :: Error -> String
displayError (Error s) = "Error: " <> s
