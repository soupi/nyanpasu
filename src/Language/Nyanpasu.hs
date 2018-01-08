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
  , getResult
  )
where

import System.IO
import System.Exit
import Control.Monad
import Text.Groom
import Data.Bifunctor

import Language.Nyanpasu.Samples
import Language.X86 (Instruction(IJmp))
import Language.Nyanpasu.Utils (readFail)
import Language.Nyanpasu.Options as Export
import Language.Nyanpasu.IR as Export
import Language.Nyanpasu.Error as Export
import qualified Language.Nyanpasu.IR.CodeGen as CG
import qualified Language.X86 as X86

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
    Right (CG.Assembly rs) ->
      putStrLn rs

compileExprX86 :: Expr () -> IO ()
compileExprX86 expr =
  case CG.compileProgram (Program [] expr) of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right (CG.Assembly rs) ->
      putStrLn rs

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
  case getResult =<< x86InterpretExpr expr of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

interpretX86 :: Program () -> IO ()
interpretX86 program =
  case getResult =<< x86Interpret program of
    Left err -> do
      hPutStrLn stderr (displayError err)
      exitFailure
    Right rs ->
      print rs

-- | Compile and interpret an AST.Expr
x86InterpretExpr :: Expr () -> Either Error X86.Machine
x86InterpretExpr =
  runX86Interpreter
  <=< CG.compileProgramRawVM
    . Program []

-- | Compile and interpret an Program
x86Interpret :: Program () -> Either Error X86.Machine
x86Interpret =
  runX86Interpreter
    . (IJmp (CG.lblToAddr ("my_code",Nothing)):)
  <=< CG.compileProgramRawVM

runX86Interpreter :: [Instruction] -> Either Error X86.Machine
runX86Interpreter =
  first AsmError
    . (X86.getMachine <=< X86.interpret . (:[]) . X86.initMachine . X86.toCode)

getResult :: X86.Machine -> Either Error (Atom ())
getResult = int32ToVal () . X86.getReg X86.EAX
