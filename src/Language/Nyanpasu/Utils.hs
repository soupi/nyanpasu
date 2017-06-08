module Language.Nyanpasu.Utils where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Typeable (Typeable, typeOf)
import Text.Read (readMaybe)

import Debug.Trace (trace)
import Text.Groom (groom)

import Language.Nyanpasu.Error

lookupM :: (MonadError (Error ann) m, MonadState s m) => (s -> M.Map String v) -> String -> m v
lookupM getter key = do
  env <- getter <$> get
  case M.lookup key env of
    Nothing ->
      throwErr $ "Undefined variable '" <> key <> "'."
    Just v ->
      pure v

llookupM :: (MonadError (Error ann) m, MonadState s m) => (s -> [(String, v)]) -> String -> m v
llookupM getter key = do
  env <- getter <$> get
  case lookup key env of
    Nothing ->
      throwErr $ "Undefined variable '" <> key <> "'."
    Just v ->
      pure v

lookupErr :: (Show k, Ord k, MonadError (Error ann) m) => k -> M.Map k v -> m v
lookupErr key env = do
  case M.lookup key env of
    Nothing ->
      throwErr $ "Undefined variable '" <> show key <> "'."
    Just v ->
      pure v

readFail :: (Typeable a, Read a) => String -> IO a
readFail str =
  let ras = readMaybe str
  in case ras of
    Just x -> pure x
    Nothing ->
      error $ unlines
        [ "Could not read the string:"
        , "  " ++ concatMap pad str
        , "as the type:"
        , "  " ++ show (typeOf $ fromJust ras)
        ]
  where
    pad = \case
      '\n' -> "\n  "
      x -> [x]

-- | Function composition of 2
{-# INLINE (.*) #-}
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) f g = \x y -> f (g x y)

infixr 9 .*

-- | trace with label
traceL :: Show a => String -> a -> a
traceL s x = trace (s ++ ": " ++ groom x) x
