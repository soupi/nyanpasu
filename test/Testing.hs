module Testing
  ( module Testing
  ) where

import Data.Function as Testing
import Test.Tasty as Testing
import Test.Tasty.HUnit as Testing hiding ((@=?))
import Test.Tasty.QuickCheck as Testing
import Text.Groom

import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST (Expr)
import qualified Language.Nyanpasu.LL.Interpreter as LLI
import qualified Language.Nyanpasu.Assembly.X86 as X86

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
  (Either (Error ()) LLI.Val -> Either (Error ()) LLI.Val -> t) -> Expr () -> t
compareProgram cmp e =
  (LLI.int32ToVal =<< X86.interpret e) `cmp` LLI.interpret e
