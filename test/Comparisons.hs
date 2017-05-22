module Comparisons where

import Testing

import Data.Bits
import Language.Nyanpasu.Types
import Language.Nyanpasu.LL.AST

tests :: TestTree
tests =
  testGroup "Comparisons" $
    mconcat
      [ concatMap (\(fn, f) -> zipWith (\n t -> testProperty ("QuickCheck " ++ fn ++ " " ++ show n) t) [1..] (qc f)) comps
      , concatMap (\(fn, f) -> zipWith (\n t -> testProperty ("QuickCheck1 " ++ fn ++ " " ++ show n) t) [1..] (qc1 f)) comps
      , concatMap (\(fn, f) -> zipWith (\n t -> testCase ("Eq " ++ fn ++ " " ++ show n) t) [1..] (unit f)) comps
      ]

qc :: (Expr () -> Expr () -> Expr ()) -> [Int32 -> Int32 -> Bool]
qc f =
  [ \((`shiftR` 1) -> i) ((`shiftR` 1) -> j) -> compareProgram (==) $ f (num_ i) (num_ j)
  ]

qc1 :: (Expr () -> Expr () -> Expr ()) -> [Int32 -> Bool]
qc1 f =
  [ \((`shiftR` 1) -> i) -> compareProgram (==) $ f (num_ i) (num_ i)
  , \((`shiftR` 1) -> i) -> compareProgram (==) $ f (num_ $ i + 1) (num_ i)
  , \((`shiftR` 1) -> i) -> compareProgram (==) $ f (num_ i) (num_ $ i + 1)
  ]

unit :: (Expr () -> Expr () -> Expr ()) -> [Assertion]
unit f =
  [ compareProgram (@=?) $ f (num_ 7) (num_ 7)
  , compareProgram (@=?) $ f (num_ 7) (num_ 8)
  , compareProgram (@=?) $ f (num_ 9) (num_ 4)
  , compareProgram (@=?) $ f (inc_ $ num_ 7) (inc_ $ num_ 7)
  , compareProgram (@=?) $ f (inc_ $ num_ 7) (inc_ $ num_ 8)
  , compareProgram (@=?) $ f (inc_ $ num_ 9) (inc_ $ num_ 4)
  , compareProgram (@=?) $ or_ (f (inc_ $ num_ 7) (inc_ $ num_ 7)) (f (inc_ $ num_ 7) (inc_ $ num_ 7))
  , compareProgram (@=?) $ or_ (f (inc_ $ num_ 8) (inc_ $ num_ 7)) (f (inc_ $ num_ 7) (inc_ $ num_ 7))
  , compareProgram (@=?) $ or_ (f (inc_ $ num_ 8) (inc_ $ num_ 7)) (f (inc_ $ num_ 8) (inc_ $ num_ 7))
  , compareProgram (@=?) $ f (num_ $ 356085996 `shiftR` 1) (num_ $ (-1791397651) `shiftR` 1)
  ]

comps :: [(String, Expr () -> Expr () -> Expr ())]
comps =
  [ ("eq",        eq_)
  , ("less",      less_)
  , ("greater",   greater_)
  , ("lessEq",    lessEq_)
  , ("greaterEq", greaterEq_)
  ]
