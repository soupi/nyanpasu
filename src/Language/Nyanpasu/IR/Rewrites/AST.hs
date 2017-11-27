
module Language.Nyanpasu.IR.Rewrites.AST
  ( rewrites
  , module AST
  )
where

import Control.Monad.Except

import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.AST as AST

rewrites :: Expr () -> Except Error (Expr ())
rewrites =
  pure
