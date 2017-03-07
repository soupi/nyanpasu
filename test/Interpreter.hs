
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

  ]
