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
  , x86InterpretExpr
  )
where

import System.IO
import System.Exit
import Control.Monad
import Text.Groom

import Language.Nyanpasu.Samples
import Language.Nyanpasu.Assembly.X86 (Instruction(IJmp))
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
    CompileExpr -> do
      compileExprX86 =<< readFail =<< getContents

    CompileProgram -> do
      compileX86 =<< readFail =<< getContents

    Interpret -> do
      interpreter =<< readFail @(Expr ()) =<< getContents

    CompileAndInterpret -> do
      interpretExprX86 =<< readFail =<< getContents

    CompileAndInterpretProgram -> do
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

    SamplePrograms esc ->
      putStrLn $ unlines $ map (escape esc . show) samplePrograms
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


    AnfProgram -> do
      program <- readFail =<< getContents
      case rewrites program of
        Left err -> do
          hPutStrLn stderr (displayError err)
          exitFailure
        Right rs ->
          putStrLn $ groom rs

compileX86 :: Program () -> IO ()
compileX86 program =
  case CG.compileProgram program of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

compileExprX86 :: Expr () -> IO ()
compileExprX86 expr =
  case CG.compileProgram (Program [] expr) of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

interpreter :: Expr () -> IO ()
interpreter expr =
  case x86InterpretExpr expr of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      putStrLn (show rs)

interpretExprX86 :: Expr () -> IO ()
interpretExprX86 expr =
  case int32ToVal () =<< x86InterpretExpr expr of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

interpretX86 :: Program () -> IO ()
interpretX86 program =
  case int32ToVal () =<< x86Interpret program of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

-- | Compile and interpret an AST.Expr
x86InterpretExpr :: Expr () -> Either Error Int32
x86InterpretExpr = X86.runInterpreter <=< CG.compileProgramRaw . Program []

-- | Compile and interpret an Program
x86Interpret :: Program () -> Either Error Int32
x86Interpret =
  X86.runInterpreter
  . (IJmp ("my_code",Nothing):)
  <=< CG.compileProgramRaw
