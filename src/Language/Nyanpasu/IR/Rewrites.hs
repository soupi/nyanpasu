module Language.Nyanpasu.IR.Rewrites where

import Language.Nyanpasu.Types
import Language.Nyanpasu.Error
import qualified Language.Nyanpasu.IR.Rewrites.ANF as ANF
import qualified Language.Nyanpasu.IR.Rewrites.AST as AST

import Control.Monad.Except

rewrites :: AST.Expr () -> Either Error (ANF.Expr Int32)
rewrites expr = runExcept
  $ pure expr
  >>= AST.rewrites
  >>= ANF.rewrites
