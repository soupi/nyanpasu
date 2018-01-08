module Testing
  ( module Testing
  ) where

import Data.Function as Testing
import Test.Tasty as Testing
import Test.Tasty.HUnit as Testing hiding ((@=?))
import Test.Tasty.QuickCheck as Testing
import Text.Groom

import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.AST (Expr, Atom)
import qualified Language.Nyanpasu.IR.Interpreter as IR
import Language.Nyanpasu (x86InterpretExpr, getResult)
import qualified Language.X86 as X86

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


assertCmp ::  Either Error X86.Machine -> Either Error (Atom ()) -> Assertion
assertCmp x y =
  if y == (getResult =<< x)
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
        , "" ++ groom (getResult =<< x)
        , ""
        , "Machine:"
        , "========"
        , ""
        , groom x
        ]

compareProgramVM :: Expr () -> Assertion
compareProgramVM e =
  assertCmp
    (x86InterpretExpr e)
    (IR.interpret e)

compareProgram ::
  (Either Error (Atom ()) -> Either Error (Atom ()) -> t) -> Expr () -> t
compareProgram cmp e =
  (getResult =<< x86InterpretExpr e) `cmp` IR.interpret e
