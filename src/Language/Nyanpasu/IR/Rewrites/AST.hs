
module Language.Nyanpasu.IR.Rewrites.AST
  ( rewrites
  , module AST
  )
where

import Control.Monad.Except

import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.AST as AST
import Language.Nyanpasu.IR.Rewrites.AST.LambdaLifting

rewrites :: Program () -> Except Error (Program ())
rewrites =
  pure . lambdaLifting
