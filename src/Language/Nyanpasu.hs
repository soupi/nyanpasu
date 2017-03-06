module Language.Nyanpasu
  ( module Export
  , run
  )
where

import System.IO
import System.Exit
import System.Environment

import Language.Nyanpasu.LL as Export
import Language.Nyanpasu.Error as Export

run :: IO ()
run = do
  getArgs >>= \case
    ["samples"] ->
      putStrLn $ unlines $ map show samples

    [expr] ->
      case compileProgram (read expr) of
        Left err -> do
          hPutStrLn stderr (displayError err)
          exitFailure
        Right rs ->
          putStrLn rs

    _ -> do
      hPutStrLn stderr "Usage: nyanpasu \"<expr>\""
      exitFailure
      

samples :: [Expr]
samples =
  [ Num 7
  , Inc $ Dec $ Dec $ Inc $ Dec $ Inc $ Inc $ Num 7
  , Let "x" (Dec $ Num 1) (Idn "x")
  , Let "x" (Dec $ Num 1) (Let "x" (Inc $ Inc $ Idn "x") (Inc $ Idn "x"))
  , Let
      "a"
      (Num 10)
      $ Let
          "c"
          (Let
            "b"
            (Inc $ Idn "a")
            (Let
              "d"
              (Inc $ Idn "b")
              (Inc $ Idn "b")
            )
          )
        (Inc $ Idn "c")
  ]
