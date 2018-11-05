module Programs where

import Testing
import Language.Nyanpasu.IR.AST
import Language.Nyanpasu.Types


tests :: TestTree
tests =
  testGroup "Programs" $
    mconcat
      -- [ zipWith (\n t -> testCase ("Programs " ++ show n) t) [1..] sampleProgramsAndResults
      [ zipWith (\n t -> testCase ("Programs " ++ show n) t) [1..] [atest]
      ]

atest =

  compareProgramVM' (num_ 7) $ Program
    []
    (fun_ "assaf" ["x"] (idn_ "x") $ call_ "assaf" [num_ 7])
  
sampleProgramsAndResults :: [Assertion]
sampleProgramsAndResults =
  [ compareProgramVM' (num_ 7) $ Program
    []
    $ val_
      "a"
      true_
      $ if_
        (idn_ "a")
        (if_ false_ (num_ 5) (num_ 7))
        (num_ 0)
  
  , compareProgramVM' (num_ 7) $ Program
    []
    (fun_ "assaf" ["x"] (idn_ "x") $ call_ "assaf" [num_ 7])
  
  , compareProgramVM' (num_ 7) $ Program
    [def_ "id" ["x"] (idn_ "x")]
    (call_ "id" [num_ 7])

  , compareProgramVM' (num_ 7) $ Program
    [def_ "const" ["x", "y"] (idn_ "x")]
    (call_ "const" [num_ 7, num_ 8])

  , compareProgramVM' (num_ 14) $ Program
    [def_ "double" ["x"] (add_ (idn_ "x") (idn_ "x"))]
    (call_ "double" [num_ 7])

  , compareProgramVM' (num_ 3) $ Program
    []
    $ val_ "x" (dec_ $ num_ 1) (val_ "x" (inc_ $ inc_ $ idn_ "x") (inc_ $ idn_ "x"))

  , compareProgramVM' (num_ 120) $ Program
    { progDefs =
      [ def_ "factorial" ["n"]
          (if_
            (eq_ (num_ 0) (idn_ "n"))
            (num_ 1)
            (mul_ (idn_ "n") (call_ "factorial" [sub_ (idn_ "n") (num_ 1)])))
      ]
    , progMain = call_ "factorial" [num_ 5]
    }

  , compareProgramVM' (num_ 6) $ Program
    { progDefs =
      [ def_ "inc" ["n"]
          $ call_ "add" [idn_ "n", num_ 1]
      , def_ "add" ["x", "y"]
          $ add_ (idn_ "x") (idn_ "y")
      ]
    , progMain = call_ "inc" [num_ 5]
    }

  , compareProgramVM' (num_ 120) $ Program
    { progDefs =
      [ def_ "factorial" ["n"]
          (call_ "factorialGo" [mul_ (num_ 1) (idn_ "n"), sub_ (idn_ "n") (num_ 1)])
      , def_ "factorialGo" ["acc", "n"]
          (if_
            (eq_ (num_ 0) (idn_ "n"))
            (idn_ "acc")
            (call_ "factorialGo" [mul_ (idn_ "acc") (idn_ "n"), sub_ (idn_ "n") (num_ 1)]))
      ]
    , progMain = call_ "factorial" [num_ 5]
    }

  , compareProgramVM' false_ $ Program
    { progDefs =
      [ def_ "even" ["n"]
          (if_
            (eq_ (num_ 0) (idn_ "n"))
            true_
            (call_ "odd" [sub_ (idn_ "n") (num_ 1)]))

      , def_ "odd" ["n"]
          (if_
            (eq_ (num_ 0) (idn_ "n"))
            false_
            (call_ "even" [sub_ (idn_ "n") (num_ 1)]))
      ]
    , progMain = call_ "even" [num_ 51]
    }

  , compareProgramVM' (num_ 51) $ Program
    { progDefs =
      [ def_ "id1" ["n"]
          $ call_ "id2" [idn_ "n"]

      , def_ "id2" ["n"]
          $ call_ "id3" [idn_ "n"]

      , def_ "id3" ["n"]
          $ idn_ "n"

      ]
    , progMain = call_ "id1" [num_ 51]
    }

  , compareProgramVM' (num_ 2) $ Program [] $ snd_ $ pair_ (num_ 1) (num_ 2)
  , compareProgramVM' (num_ 2) $ Program [] $ fst_ $ snd_ $ pair_ (num_ 1) $ pair_ (num_ 2) (num_ 3)
  ]
