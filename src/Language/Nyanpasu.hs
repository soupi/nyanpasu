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
      case compileProgram (read expr :: Expr ()) of
        Left err -> do
          hPutStrLn stderr (displayError err)
          exitFailure
        Right rs ->
          putStrLn rs

    _ -> do
      hPutStrLn stderr "Usage: nyanpasu \"<expr>\""
      exitFailure


samples :: [Expr ()]
samples =
  [ num 7
  , inc $ dec $ dec $ inc $ dec $ inc $ inc $ num 7
  , let' "x" (dec $ num 1) (idn "x")
  , let' "x" (dec $ num 1) (let' "x" (inc $ inc $ idn "x") (inc $ idn "x"))
  , let'
      "a"
      (num 10)
      $ let'
          "c"
          (let'
            "b"
            (inc $ idn "a")
            (let'
              "d"
              (inc $ idn "b")
              (inc $ idn "b")
            )
          )
        (inc $ idn "c")
  ]
