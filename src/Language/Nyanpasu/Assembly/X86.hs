{- | Code generation for X86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, NegativeLiterals #-}

module Language.Nyanpasu.Assembly.X86 where

import Language.Nyanpasu.Types

import Data.Data
import GHC.Generics
import Control.DeepSeq

import Data.Char (toLower)
import Data.Monoid

-- import Text.Groom
-- import Debug.Trace

--------------
-- Assembly --
--------------

-- | The Instruction type
--   represents an x86 assembly instruction
data Instruction
  = IMov Arg Arg
  | IAdd Arg Arg
  | ISub Arg Arg
  | IMul Arg Arg
  | ICmp Arg Arg
  | IXor Arg Arg
  | IAnd Arg Arg
  | IOr  Arg Arg
  | IShl Arg Arg
  | IShr Arg Arg
  | ISar Arg Arg
  | ISal Arg Arg
  | ITest Arg Arg
  | IJmp  Label
  | IJe   Label
  | IJnz  Label
  | IJz   Label
  | Label Label
  | ICall Label
  | IRet
  | IPush Arg
  | IPop  Arg
  | EmptyInst
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

type Label = (String, Maybe Int32)

-- | The Arg type
--   represents an x86 assembly argument to an instruction
data Arg
  = Const Int32
  | Reg Reg
  | RegOffset Reg Int32
  | ArgTimes Int32 Arg
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Reg type
--   represents an x86 assembly register
data Reg
  = EAX
  | EBX
  | ECX
  | EDX
  | ESP
  | EBP
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

-- | Output
newtype Assembly = Assembly String
  deriving (Eq, Ord, Generic, NFData, Data, Typeable)

instance Show Assembly where
  show (Assembly asmStr) = asmStr

-----------------
-- PP Assembly --
-----------------

-- | Pretty print a list of instructions to an assembly string
ppAsm :: [Instruction] -> String
ppAsm = unlines . map ppInstruction

-- | Pretty print an instruction to an assembly string
ppInstruction :: Instruction -> String
ppInstruction = \case
  IMov dest src ->
    ppOp "mov" dest src
  IAdd dest src ->
    ppOp "add" dest src
  ISub dest src ->
    ppOp "sub" dest src
  IMul dest src ->
    ppOp "mul" dest src
  IXor dest src ->
    ppOp "xor" dest src
  IAnd dest src ->
    ppOp "and" dest src
  IOr  dest src ->
    ppOp "or"  dest src
  ICmp dest src ->
    ppOp "cmp" dest src
  ITest dest src ->
    ppOp "test" dest src
  IShr dest src ->
    ppOp "shr" dest src
  ISar dest src ->
    ppOp "sar" dest src
  IShl dest src ->
    ppOp "shl" dest src
  ISal dest src ->
    ppOp "sal" dest src
  IJmp lbl ->
    "jmp " <> ppLabel lbl
  IJe  lbl ->
    "je " <> ppLabel lbl
  IJnz lbl ->
    "jnz " <> ppLabel lbl
  IJz lbl ->
    "jz " <> ppLabel lbl
  Label lbl ->
    ppLabel lbl <> ":"
  IRet ->
    "ret"
  ICall lbl ->
    "call " <> ppLabel lbl
  IPush arg ->
    "push " <> ppArg arg
  IPop arg ->
    "pop " <> ppArg arg
  EmptyInst ->
    ""

-- | Pretty print a label
ppLabel :: Label -> String
ppLabel (lbl, mi) = case mi of
  Just i ->
    lbl <> "_" <> show i
  Nothing ->
    lbl

-- | Pretty print an operation with two arguments
ppOp :: String -> Arg -> Arg -> String
ppOp cmd dest src =
    unwords
      [ cmd
      , ppArg dest <> ","
      , ppArg src
      ]

-- | Pretty print an argument
ppArg :: Arg -> String
ppArg = \case
  Const i -> show i
  Reg r   -> ppReg r
  RegOffset reg addr ->
    "[" <> ppReg reg <> " - 4*" <> show addr <> "]"
  ArgTimes n arg ->
    show n <> "*" <> ppArg arg


-- | Pretty print a register
ppReg :: Reg -> String
ppReg = map toLower . show

