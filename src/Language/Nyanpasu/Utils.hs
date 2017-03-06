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
