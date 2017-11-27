{-# LANGUAGE TypeApplications #-}

{- | Entry point
-}

module Language.Nyanpasu
  ( module Export
  , run
  , compileX86
  , interpretX86
  , interpreter
  , samples
  , x86Interpret
  )
where

import System.IO
import System.Exit
import Control.Monad

import Language.Nyanpasu.Types
import Language.Nyanpasu.Utils (readFail)
import Language.Nyanpasu.Options as Export
import Language.Nyanpasu.IR as Export
import Language.Nyanpasu.Error as Export
import qualified Language.Nyanpasu.IR.CodeGen as CG
import qualified Language.Nyanpasu.Assembly.X86.Interpreter as X86

run :: IO ()
run = do
  parseArgs >>= \case
    Compile -> do
      compileX86 =<< readFail =<< getContents

    Interpret -> do
      interpreter =<< readFail @(Expr ()) =<< getContents

    CompileAndInterpret -> do
      interpretX86 =<< readFail =<< getContents

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

compileX86 :: Expr () -> IO ()
compileX86 expr =
  case CG.compileProgram expr of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

interpreter :: Expr () -> IO ()
interpreter expr =
  case x86Interpret expr of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      putStrLn (show rs)

interpretX86 :: Expr () -> IO ()
interpretX86 expr =
  case int32ToVal () =<< x86Interpret expr of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

-- | Compile and interpret an AST.Expr
x86Interpret :: Expr () -> Either Error Int32
x86Interpret = X86.runInterpreter <=< CG.compileExprRaw

samples :: [Expr ()]
samples =
  [ num_ 7
  , inc_ $ dec_ $ dec_ $ inc_ $ dec_ $ inc_ $ inc_ $ num_ 7
  , let_ "x" (dec_ $ num_ 1) (idn_ "x")
  , let_ "x" (dec_ $ num_ 1) (let_ "x" (inc_ $ inc_ $ idn_ "x") (inc_ $ idn_ "x"))
  , let_
      "a"
      (num_ 10)
      $ let_
          "c"
          (let_
            "b"
            (inc_ $ idn_ "a")
            (let_
              "d"
              (inc_ $ idn_ "b")
              (inc_ $ idn_ "b")
            )
          )
        (inc_ $ idn_ "c")
  , let_
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
  ]

