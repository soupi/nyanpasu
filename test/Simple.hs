module Simple where

import Testing
import Data.Int (Int16)

import Language.Nyanpasu (samples)
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST
import qualified Language.Nyanpasu.LL.Interpreter as LLI
import qualified Language.Nyanpasu.Assembly.X86 as X86

tests :: TestTree
tests =
  testGroup "Simple" $
    mconcat
      [ zipWith (\n t -> testProperty ("QuickCheck " ++ show n) t) [1..] qc
      , zipWith (\n t -> testCase ("Simple " ++ show n) t) [1..] simple
      ]

compareProgram ::
  (Either Error LLI.Val -> Either Error LLI.Val -> t) -> Expr () -> t
compareProgram cmp e =
  (LLI.int32ToVal =<< X86.interpret e) `cmp` LLI.interpret e

qc :: [(Int16 -> Bool)]
qc =
  [ \i -> compareProgram (==) (num_ $ fromIntegral i)
  ]

simple :: [Assertion]
simple =
  [ compareProgram (@=?) (inc_ $ num_ 7)

  , compareProgram (@=?) (dec_ $ num_ 7)

  , compareProgram (@=?) (inc_ $ inc_ $ dec_ $ num_ 7)

  , compareProgram (@=?)
      (let_
         "a"
         true_
         $ if_
           (idn_ "a")
           (if_ false_ (num_ 5) (num_ 7))
           (num_ 0))

  , compareProgram (@=?)
      (add_
        (add_ (inc_ $ num_ 2) (inc_ $ num_ 2))
        (add_ (inc_ $ num_ 2) (inc_ $ num_ 2))
      )
  ]
  ++ map (compareProgram (@=?)) samples
