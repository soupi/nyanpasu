{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86 where

import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.LL.CodeGenUtils

import Data.Data
import GHC.Generics
import Control.DeepSeq

import Data.Char (toLower)
import Data.Monoid
import Control.Monad.State
import Control.Monad.Except

--------------
-- Assembly --
--------------

-- | The Instruction type
--   represents an x86 assembly instruction
data Instruction
  = IMov Arg Arg
  | IAdd Arg Arg
  | ISub Arg Arg
  | ICmp Arg Arg
  | IJmp  String Int
  | IJe   String Int
  | Label String Int
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Arg type
--   represents an x86 assembly argument to an instruction
data Arg
  = Const Int
  | Reg Reg
  | RegOffset Reg Int
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Reg type
--   represents an x86 assembly register
data Reg
  = EAX
  | ESP
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)


---------------------
-- Code Generation --
---------------------

compileProgram :: Expr () -> Either Error String
compileProgram (assignLabels -> e) = runExcept . flip evalStateT initState $ do
  compiled <- compileExpr e
  asmStr   <- ppAsm compiled
  pure $
    unlines
      [ prelude
      , asmStr
      , suffix
      ]

  where
    prelude =
      unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        ]
    suffix = "ret"


compileExpr :: Expr Int -> CodeGen [Instruction]
compileExpr = \case
  Num _ i ->
    pure [ IMov (Reg EAX) (Const i) ]

  PrimOp _ prim -> case prim of
    Inc e -> do
      rest <- compileExpr e
      pure $
        rest <> [ IAdd (Reg EAX) (Const 1) ]

    Dec e -> do
      rest <- compileExpr e
      pure $
        rest <> [ ISub (Reg EAX) (Const 1) ]

  Let _ name binder body -> do
    asm <- concat <$> sequence
      [ compileExpr binder
      , do addr <- insertVar name
           pure [ IMov (RegOffset ESP addr) (Reg EAX) ]
      , compileExpr body
      ]
    popVar
    pure asm

  Idn _ name -> do
    addr <- llookupM cgSymbols name
    pure [ IMov (Reg EAX) (RegOffset ESP addr) ]

  If path test true false -> do
   testAsm  <- compileExpr test
   trueAsm  <- compileExpr true
   falseAsm <- compileExpr false
   pure $ concat
     [ testAsm
     , [ ICmp (Reg EAX) (Const 0)
       , IJe  "if_false" path
       , Label "if_true" path
       ]
     , trueAsm
     , [ IJmp  "if_done"  path
       , Label "if_false" path
       ]
     , falseAsm
     ,  [ Label "if_done" path ]
     ]
   

ppAsm :: [Instruction] -> CodeGen String
ppAsm = pure . unlines . map ppInstruction

ppInstruction :: Instruction -> String
ppInstruction = \case
  IMov dest src ->
    ppOp "mov" dest src
  IAdd dest src ->
    ppOp "add" dest src
  ISub dest src ->
    ppOp "sub" dest src
  ICmp dest src ->
    ppOp "cmp" dest src
  IJmp lbl i ->
    "jmp " <> lbl <> "_" <> show i
  IJe  lbl i ->
    "je " <> lbl <> "_" <> show i
  Label lbl i ->
    lbl <> "_" <> show i <> ":"


ppOp :: String -> Arg -> Arg -> String
ppOp cmd dest src =
    unwords
      [ cmd
      , ppArg dest <> ","
      , ppArg src
      ]

ppArg :: Arg -> String
ppArg = \case
  Const i -> show i
  Reg r   -> ppReg r
  RegOffset reg addr ->
    "[" <> ppReg reg <> " - 4*" <> show addr <> "]"

ppReg :: Reg -> String
ppReg = map toLower . show

