
module Interpreter where

import Testing

import Language.Nyanpasu

tests :: TestTree
tests =
  testGroup "Interpreter" $
    mconcat
      [ zipWith (\n t -> testCase ("Interpreter " ++ show n) t) [1..] inter
      ]

inter :: [Assertion]
inter =
  [ interpret (Num 5) @=?
      Right 5

  , interpret (Dec $ Dec $ Num 8) @=?
      Right 6

  , interpret (Inc $ Dec $ Num 7) @=?
      Right 7

  , interpret (Let "x" (Inc $ Num 6) $ Dec $ Inc $ Inc $ Idn "x") @=?
      Right 8

  , interpret (Let "x" (Inc $ Num 6) $ Let "x" (Dec $ Dec $ Num 1) $ Inc $ Idn "x") @=?
      Right 0
  , interpret (Let "x" (Inc $ Num 6) $ Let "y" (Dec $ Dec $ Num 1) $ Inc $ Idn "x") @=?
      Right 8

  ]
