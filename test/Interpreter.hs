
module Interpreter where

import Testing
import Control.Monad
-- import Data.Bits

-- import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.AST
import qualified Language.Nyanpasu.IR.Interpreter as IR
import Language.Nyanpasu (x86InterpretExpr, getResult)

tests :: TestTree
tests =
  testGroup "Interpreter" $
    mconcat
      [ zipWith (\n t -> testCase ("IR.Interpreter " ++ show n) t) [1..] (inter IR.interpret)
      , zipWith (\n t -> testCase ("X86.Interpreter " ++ show n) t) [1..] (inter (getResult <=< x86InterpretExpr))
      ]

inter :: (Expr () -> Either Error (Atom ())) -> [Assertion]
inter interpret =
  [ interpret (num_ 5) @=?
      Right (Num () 5)

  , interpret (dec_ $ dec_ $ num_ 8) @=?
      Right (Num () 6)

  , interpret (inc_ $ dec_ $ num_ 7) @=?
      Right (Num () 7)

  , interpret (val_ "x" (inc_ $ num_ 6) $ dec_ $ inc_ $ inc_ $ idn_ "x") @=?
      Right (Num () 8)

  , interpret (val_ "x" (inc_ $ num_ 6) $ val_ "x" (dec_ $ dec_ $ num_ 1) $ inc_ $ idn_ "x") @=?
      Right (Num () 0)
  , interpret (val_ "x" (inc_ $ num_ 6) $ val_ "y" (dec_ $ dec_ $ num_ 1) $ inc_ $ idn_ "x") @=?
      Right (Num () 8)

  , interpret
      (val_
        "a"
        true_
        $ if_
          (idn_ "a")
          (if_ false_ (num_ 5) (num_ 7))
          (num_ 0))
      @=?
      Right (Num () 7)

  ]
