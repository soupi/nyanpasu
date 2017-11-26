module Testing
  ( module Testing
  ) where

import Data.Function as Testing
import Test.Tasty as Testing
import Test.Tasty.HUnit as Testing hiding ((@=?))
import Test.Tasty.QuickCheck as Testing
import Text.Groom

import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.AST (Expr)
import qualified Language.Nyanpasu.IR.Interpreter as IR
import Language.Nyanpasu (x86Interpret)

assertEq :: (Eq a, Show a) => (a, a) -> Assertion
assertEq (x,y) =
  if y == x
    then
      pure ()
    else do
      errorWithoutStackTrace $ unlines
        [ ""
        , ""
        , "Expected:"
        , "========="
        , ""
        , "" ++ groom y
        , ""
        , "But got:"
        , "========"
        , ""
        , "" ++ groom x
        ]

(@=?) :: (Eq a, Show a) => a -> a -> Assertion
x @=? y = assertEq (x,y)


compareProgram ::
  (Either (Error ()) IR.Val -> Either (Error ()) IR.Val -> t) -> Expr () -> t
compareProgram cmp e =
  (IR.int32ToVal =<< x86Interpret e) `cmp` IR.interpret e
