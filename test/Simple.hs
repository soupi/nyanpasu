module Simple where

import Testing

import Language.Nyanpasu

tests :: TestTree
tests =
  testGroup "Simple" $
    mconcat
      [ zipWith (\n t -> testProperty ("QuickCheck " ++ show n) t) [1..] qc
      , zipWith (\n t -> testCase ("Simple " ++ show n) t) [1..] simple
      ]

qc :: [(Int -> Bool)]
qc =
  [ \i ->
      compileProgram (num i) ==
      Right (Assembly $ unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, " ++ show i
        , ""
        , "ret"
        ])
  ]

simple :: [Assertion]
simple =
  [ compileProgram (inc $ num 7) @=?
      Right (Assembly $ unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 7"
        , "add eax, 1"
        , ""
        , "ret"
        ])

  , compileProgram (dec $ num 7) @=?
      Right (Assembly $ unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 7"
        , "sub eax, 1"
        , ""
        , "ret"
        ])

  , compileProgram (inc $ inc $ dec $ num 7) @=?
      Right (Assembly $ unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 7"
        , "sub eax, 1"
        , "mov [esp - 4*1], eax"
        , "mov eax, [esp - 4*1]"
        , "add eax, 1"
        , "mov [esp - 4*1], eax"
        , "mov eax, [esp - 4*1]"
        , "add eax, 1"
        , ""
        , "ret"
        ])

  , compileProgram
      (let'
         "a"
         (num 1)
         $ if'
           (idn "a")
           (if' (num 0) (num 5) (num 7))
           (num 0))
      @=?
      Right (Assembly $ unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 1"
        , "mov [esp - 4*1], eax"
        , "mov eax, [esp - 4*1]"
        , "cmp eax, 0"
        , "je if_false_2"
        , "if_true_2:"
        , "mov eax, 0"
        , "cmp eax, 0"
        , "je if_false_4"
        , "if_true_4:"
        , "mov eax, 5"
        , "jmp if_done_4"
        , "if_false_4:"
        , "mov eax, 7"
        , "if_done_4:"
        , "jmp if_done_2"
        , "if_false_2:"
        , "mov eax, 0"
        , "if_done_2:"
        , ""
        , "ret"
        ])

  , compileProgram
      (add
        (add (inc $ num 2) (inc $ num 2))
        (add (inc $ num 2) (inc $ num 2))
      )
      @=?
      Right (Assembly $ unlines
       [ "section .text"
       , "global my_code"
       , "my_code:"
       , ""
       , "mov eax, 2"
       , "add eax, 1"
       , "mov [esp - 4*1], eax"
       , "mov eax, 2"
       , "add eax, 1"
       , "mov [esp - 4*2], eax"
       , "mov eax, [esp - 4*1]"
       , "add eax, [esp - 4*2]"
       , "mov [esp - 4*1], eax"
       , "mov eax, 2"
       , "add eax, 1"
       , "mov [esp - 4*2], eax"
       , "mov eax, 2"
       , "add eax, 1"
       , "mov [esp - 4*3], eax"
       , "mov eax, [esp - 4*2]"
       , "add eax, [esp - 4*3]"
       , "mov [esp - 4*2], eax"
       , "mov eax, [esp - 4*1]"
       , "add eax, [esp - 4*2]"
       , ""
       , "ret"
       ])
  ]
