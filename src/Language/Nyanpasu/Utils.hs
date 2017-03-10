module Language.Nyanpasu.Utils where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Data.Monoid

import Language.Nyanpasu.Error

lookupM :: (MonadError Error m, MonadState s m) => (s -> M.Map String v) -> String -> m v
lookupM getter key = do
  env <- getter <$> get
  case M.lookup key env of
    Nothing ->
      throwErr $ "Undefined variable '" <> key <> "'."
    Just v ->
      pure v

type Name = String

llookupM :: (MonadError Error m, MonadState s m) => (s -> [(String, v)]) -> String -> m v
llookupM getter key = do
  env <- getter <$> get
  case lookup key env of
    Nothing ->
      throwErr $ "Undefined variable '" <> key <> "'."
    Just v ->
      pure v

lookupErr :: (Show k, Ord k, MonadError Error m) => k -> M.Map k v -> m v
lookupErr key env = do
  case M.lookup key env of
    Nothing ->
      throwErr $ "Undefined variable '" <> show key <> "'."
    Just v ->
      pure v
