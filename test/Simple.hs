module Simple where

import Testing

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
  (Either Error Int -> Either Error Int -> t) -> Expr () -> t
compareProgram cmp e =
  X86.interpret e `cmp` LLI.interpret e

qc :: [(Int -> Bool)]
qc =
  [ \i -> compareProgram (==) (num i)
  ]

simple :: [Assertion]
simple =
  [ compareProgram (@=?) (inc $ num 7)

  , compareProgram (@=?) (dec $ num 7)

  , compareProgram (@=?) (inc $ inc $ dec $ num 7)

  , compareProgram (@=?)
      (let'
         "a"
         (num 1)
         $ if'
           (idn "a")
           (if' (num 0) (num 5) (num 7))
           (num 0))

  , compareProgram (@=?)
      (add
        (add (inc $ num 2) (inc $ num 2))
        (add (inc $ num 2) (inc $ num 2))
      )
  ]
  ++ map (compareProgram (@=?)) samples
