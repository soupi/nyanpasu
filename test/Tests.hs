
import Testing
import qualified Simple
import qualified Interpreter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Simple.tests
    , Interpreter.tests
    ]
