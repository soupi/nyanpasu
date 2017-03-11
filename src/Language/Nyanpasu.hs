{-# LANGUAGE TypeApplications #-}

{- | Entry point
-}

module Language.Nyanpasu
  ( module Export
  , run
  , samples
  )
where

import System.IO
import System.Exit

import Language.Nyanpasu.Utils (readFail)
import Language.Nyanpasu.Options as Export
import Language.Nyanpasu.LL as Export
import Language.Nyanpasu.Error as Export
import qualified Language.Nyanpasu.Assembly.X86 as X86

run :: IO ()
run = do
  parseArgs >>= \case
    Compile -> do
      expr <- readFail =<< getContents
      case X86.compileProgram expr of
        Left err -> do
          hPutStrLn stderr (displayError err)
          exitFailure
        Right rs ->
          print rs

    Interpret -> do
      expr <- readFail @(Expr ()) =<< getContents
      case interpret expr of
        Left err -> do
          hPutStrLn stderr (displayError err)
          exitFailure
        Right rs ->
          putStrLn (show rs)

    CompileAndInterpret -> do
      expr <- readFail =<< getContents
      case X86.interpret expr of
        Left err -> do
          hPutStrLn stderr (displayError err)
          exitFailure
        Right rs ->
          print rs

    Samples esc ->
      putStrLn $ unlines $ map (escape esc . show) samples
        where
          escape = \case
              Just Escape     -> concatMap go
              Just EscapeHard -> concatMap gohard
              Nothing    -> id
            where
              go = \case
                '"' -> [ '\\', '"' ]
                x   -> [ x ]

              gohard = \case
                '"' -> [ '\\', '\\', '\\', '"' ]
                x   -> [ x ]

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
  , let'
      "a"
      (num 1)
      $ if'
        (idn "a")
        (if' (num 0) (num 5) (num 7))
        (num 0)
  , add (num 7) (num 10)
  ]

