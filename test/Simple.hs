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
      compileProgram (Num i) ==
      Right (unlines
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
  [ compileProgram (Inc $ Num 7) @=?
      Right (unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 7"
        , "add eax, 1"
        , ""
        , "ret"
        ])

  , compileProgram (Dec $ Num 7) @=?
      Right (unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 7"
        , "sub eax, 1"
        , ""
        , "ret"
        ])

  , compileProgram (Inc $ Inc $ Dec $ Num 7) @=?
      Right (unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        , ""
        , "mov eax, 7"
        , "sub eax, 1"
        , "add eax, 1"
        , "add eax, 1"
        , ""
        , "ret"
        ])
  ]
