
import Testing
import qualified Simple
import qualified Interpreter
import qualified Comparisons
import qualified Programs

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Simple.tests
    , Interpreter.tests
    , Comparisons.tests
    , Programs.tests
    ]
