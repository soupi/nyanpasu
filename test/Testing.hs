module Testing
  ( module Testing
  ) where

import Data.Function as Testing
import Test.Tasty as Testing
import Test.Tasty.HUnit as Testing hiding ((@=?))
import Test.Tasty.QuickCheck as Testing
import qualified Test.Tasty.HUnit as HU ((@=?))

assertEq :: (Eq a, Show a) => (a, a) -> Assertion
assertEq (x,y) = y HU.@=? x

(@=?) :: (Eq a, Show a) => a -> a -> Assertion
x @=? y = assertEq (x,y)
