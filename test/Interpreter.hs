
module Interpreter where

import Testing

import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST
import qualified Language.Nyanpasu.LL.Interpreter as LLI
import qualified Language.Nyanpasu.Assembly.X86 as X86

tests :: TestTree
tests =
  testGroup "Interpreter" $
    mconcat
      [ zipWith (\n t -> testCase ("LLI.Interpreter " ++ show n) t) [1..] (inter LLI.interpret)
      , zipWith (\n t -> testCase ("X86.Interpreter " ++ show n) t) [1..] (inter X86.interpret)
      ]

inter :: (Expr () -> Either Error Int) -> [Assertion]
inter interpret =
  [ interpret (num 5) @=?
      Right 5

  , interpret (dec $ dec $ num 8) @=?
      Right 6

  , interpret (inc $ dec $ num 7) @=?
      Right 7

  , interpret (let' "x" (inc $ num 6) $ dec $ inc $ inc $ idn "x") @=?
      Right 8

  , interpret (let' "x" (inc $ num 6) $ let' "x" (dec $ dec $ num 1) $ inc $ idn "x") @=?
      Right 0
  , interpret (let' "x" (inc $ num 6) $ let' "y" (dec $ dec $ num 1) $ inc $ idn "x") @=?
      Right 8

  , interpret
      (let'
        "a"
        (num 1)
        $ if'
          (idn "a")
          (if' (num 0) (num 5) (num 7))
          (num 0))
      @=?
      Right 7

  ]
