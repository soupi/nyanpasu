
module Language.Nyanpasu.IR.Rewrites.ANF
  ( rewrites
  , module ANF
  )
where

import Language.Nyanpasu.Types
import Language.Nyanpasu.Error
import qualified Language.Nyanpasu.IR.AST as AST
import Language.Nyanpasu.IR.ANF as ANF

-- rewrites
import Language.Nyanpasu.IR.Rewrites.ANF.AstToAnf

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

rewrites :: AST.Expr () -> Except Error (Expr Int32)
rewrites =
  runExprToANF
  >=> pure . assignLabels

-- | Assign a unique label to all sub-expressions
assignLabels :: Expr () -> Expr Int32
assignLabels = flip evalState 0 . traverse go
  where
    go () = do
      i <- get
      put (i + 1)
      pure i
