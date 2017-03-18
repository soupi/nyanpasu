
module Interpreter where

import Testing
import Control.Monad
import Data.Bits

import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST
import qualified Language.Nyanpasu.LL.Interpreter as LLI
import qualified Language.Nyanpasu.Assembly.X86 as X86

tests :: TestTree
tests =
  testGroup "Interpreter" $
    mconcat
      [ zipWith (\n t -> testCase ("LLI.Interpreter " ++ show n) t) [1..] (inter LLI.interpret)
      , zipWith (\n t -> testCase ("X86.Interpreter " ++ show n) t) [1..] (inter (LLI.int32ToVal <=< X86.interpret))
      ]

inter :: (Expr () -> Either Error LLI.Val) -> [Assertion]
inter interpret =
  [ interpret (num_ 5) @=?
      Right (Num () 5)

  , interpret (dec_ $ dec_ $ num_ 8) @=?
      Right (Num () 6)

  , interpret (inc_ $ dec_ $ num_ 7) @=?
      Right (Num () 7)

  , interpret (let_ "x" (inc_ $ num_ 6) $ dec_ $ inc_ $ inc_ $ idn_ "x") @=?
      Right (Num () 8)

  , interpret (let_ "x" (inc_ $ num_ 6) $ let_ "x" (dec_ $ dec_ $ num_ 1) $ inc_ $ idn_ "x") @=?
      Right (Num () 0)
  , interpret (let_ "x" (inc_ $ num_ 6) $ let_ "y" (dec_ $ dec_ $ num_ 1) $ inc_ $ idn_ "x") @=?
      Right (Num () 8)

  , interpret
      (let_
        "a"
        true_
        $ if_
          (idn_ "a")
          (if_ false_ (num_ 5) (num_ 7))
          (num_ 0))
      @=?
      Right (Num () 7)

  ]
