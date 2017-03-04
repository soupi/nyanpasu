module Testing
  ( module Testing
  ) where

import Data.Function as Testing
import Test.Tasty as Testing
import Test.Tasty.HUnit as Testing
import Test.Tasty.QuickCheck as Testing

assertEq :: (Eq a, Show a) => (a, a) -> Assertion
assertEq (x,y) = y @=? x
