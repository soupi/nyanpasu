module Simple where

import Testing
import Data.Bits

import Language.Nyanpasu (samples)
import Language.Nyanpasu.Types
import Language.Nyanpasu.IR.AST

tests :: TestTree
tests =
  testGroup "Simple" $
    mconcat
      [ zipWith (\n t -> testProperty ("QuickCheck " ++ show n) t) [1..] qc
      , zipWith (\n t -> testCase ("Simple " ++ show n) t) [1..] simple
      ]

qc :: [(Int32 -> Bool)]
qc =
  [ \i -> compareProgram (==) (num_ $ shiftR i 1)
  , \i -> compareProgram (==) (eq_ (num_ $ shiftR i 1) (num_ $ shiftR (i + 1) 1))
  ]

simple :: [Assertion]
simple =
  [ compareProgramVM (inc_ $ num_ 7)

  , compareProgramVM (dec_ $ num_ 7)

  , compareProgramVM (inc_ $ inc_ $ dec_ $ num_ 7)

  , compareProgramVM
      (val_
         "a"
         true_
         $ if_
           (idn_ "a")
           (if_ false_ (num_ 5) (num_ 7))
           (num_ 0))

  , compareProgramVM
      (add_
        (add_ (inc_ $ num_ 2) (inc_ $ num_ 2))
        (add_ (inc_ $ num_ 2) (inc_ $ num_ 2))
      )
  ]
  ++ map compareProgramVM samples
