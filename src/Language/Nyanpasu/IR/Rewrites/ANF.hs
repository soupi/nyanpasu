
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
import Language.Nyanpasu.IR.Rewrites.ANF.TailCalls

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

rewrites :: AST.Program () -> Except Error (Program Int32)
rewrites =
  normalizeProgram
  >=> pure . assignLabels
  >=> pure . addTailCalls

-- | Assign a unique label to all sub-expressions
assignLabels :: Program () -> Program Int32
assignLabels = flip evalState 0 . traverse go
  where
    go () = do
      i <- get
      put (i + 1)
      pure i
