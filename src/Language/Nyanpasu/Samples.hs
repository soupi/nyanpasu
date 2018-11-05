module Language.Nyanpasu.Samples where

import Language.Nyanpasu.IR.AST

samples :: [Expr ()]
samples =
  [ num_ 7
  , inc_ $ dec_ $ dec_ $ inc_ $ dec_ $ inc_ $ inc_ $ num_ 7
  , val_ "x" (dec_ $ num_ 1) (idn_ "x")
  , val_ "x" (dec_ $ num_ 1) (val_ "x" (inc_ $ inc_ $ idn_ "x") (inc_ $ idn_ "x"))
  , val_
      "a"
      (num_ 10)
      $ val_
          "c"
          (val_
            "b"
            (inc_ $ idn_ "a")
            (val_
              "d"
              (inc_ $ idn_ "b")
              (inc_ $ idn_ "b")
            )
          )
        (inc_ $ idn_ "c")
  , val_
      "a"
      true_
      $ if_
        (idn_ "a")
        (if_ false_ (num_ 5) (num_ 7))
        (num_ 0)
  , add_ (num_ 7) (num_ 10)
  , eq_ (add_ (num_ 7) (num_ 10)) (num_ 17)
  , less_ (add_ (num_ 7) (num_ 10)) (num_ 17)
  , less_ (add_ (num_ 8) (num_ 10)) (num_ 17)
  , if_ (less_ (add_ (num_ 8) (num_ 10)) (num_ 17)) (num_ 1) (num_ 0)
  , if_ (eq_ (add_ (num_ 8) (num_ 10)) (num_ 17)) (num_ 1) (num_ 0)
  , if_ (eq_ (add_ (num_ 7) (num_ 10)) (num_ 17)) (num_ 1) (num_ 0)
  , eq_ (add_ (num_ 8) (num_ 10)) (num_ 17)
  , if_ true_ (num_ 1) (num_ 0)
  ]

samplePrograms :: [Program ()]
samplePrograms =
  [ Program
    []
    $ val_
      "a"
      true_
      $ if_
        (idn_ "a")
        (if_ false_ (num_ 5) (num_ 7))
        (num_ 0)
  , Program
    [def_ "id" ["x"] (idn_ "x")]
    (call_ "id" [num_ 7])

  , Program
    [def_ "const" ["x", "y"] (idn_ "x")]
    (call_ "const" [num_ 7, num_ 8])

  , Program
    [def_ "double" ["x"] (add_ (idn_ "x") (idn_ "x"))]
    (call_ "double" [num_ 7])

  , Program
    []
    $ val_ "x" (dec_ $ num_ 1) (val_ "x" (inc_ $ inc_ $ idn_ "x") (inc_ $ idn_ "x"))

  , Program
    { progDefs =
      [ def_ "factorial" ["n"]
          (if_
            (eq_ (num_ 0) (idn_ "n"))
            (num_ 1)
            (mul_ (idn_ "n") (call_ "factorial" [sub_ (idn_ "n") (num_ 1)])))
      ]
    , progMain = call_ "factorial" [num_ 5]
    }

  , Program
    { progDefs =
      [ def_ "inc" ["n"]
          $ call_ "add" [idn_ "n", num_ 1]
      , def_ "add" ["x", "y"]
          $ add_ (idn_ "x") (idn_ "y")
      ]
    , progMain = call_ "inc" [num_ 5]
    }

  , Program
    { progDefs =
      [ def_ "loop" ["a", "b", "c"]
          $ call_ "loop" [idn_ "a", idn_ "b", idn_ "c"]
      ]
    , progMain = call_ "loop" [num_ 1, num_ 2, num_ 3]
    }

  , Program
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

  , Program [] $ ccall_ "print" [num_ 7]

  , Program
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

  , Program [] $ val_ "x" (num_ 7) (val_ "y" (val_ "x" (num_ 1) (idn_ "x")) (add_ (idn_ "x") (idn_ "y")))

  , Program
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

  , Program [] $ pair_ (num_ 1) (num_ 2)
  , Program [] $ snd_ $ pair_ (num_ 1) (num_ 2)
  , Program [] $ pair_ (num_ 1) $ pair_ (num_ 2) (num_ 3)
  , Program [] $ fst_ $ snd_ $ pair_ (num_ 1) $ pair_ (num_ 2) (num_ 3)
  ]
