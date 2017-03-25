{- | Code generation for X86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86.CodeGen where

import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.ANF
import Language.Nyanpasu.LL.CodeGenUtils
import qualified Language.Nyanpasu.LL.AST as AST

import Data.Data
import GHC.Generics
import Control.DeepSeq

import Data.Bits
import Data.Bool
import Data.Char (toLower)
import Data.Monoid
import Control.Monad.State
import Control.Monad.Except

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
  | IJmp  Label
  | IJe   Label
  | Label Label
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

type Label = (String, Int32)

-- | The Arg type
--   represents an x86 assembly argument to an instruction
data Arg
  = Const Int32
  | Reg Reg
  | RegOffset Reg Int32
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

-- | The Reg type
--   represents an x86 assembly register
data Reg
  = EAX
  | EBX
  | ECX
  | ESP
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

-- | Output
newtype Assembly = Assembly String
  deriving (Eq, Ord, Generic, NFData, Data, Typeable)

instance Show Assembly where
  show (Assembly asmStr) = asmStr

---------------------
-- Code Generation --
---------------------

-- | Compile an expression and output an assembly string
compileProgram :: AST.Expr () -> Either Error Assembly
compileProgram expr = do
    asmStr <- ppAsm <$> compileExprRaw expr
    pure $ Assembly $
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

-- | Compile an expression and output a assembly list of instructions
compileExprRaw :: AST.Expr () -> Either Error [Instruction]
compileExprRaw expr = do
  e <- {- (\e -> trace (groom e) e) <$> -}
    runExprToANF expr
  runExcept . flip evalStateT initState $ do
    compileExpr e

-- | Compile an expression to a list of instructions
compileExpr :: Expr Int32 -> CodeGen [Instruction]
compileExpr = \case
  Atom atom -> case atom of
    Idn _ addr -> do
      pure [ IMov (Reg EAX) (RegOffset ESP addr) ]

    _ -> do
      res <- compileAtom atom
      pure [ IMov (Reg EAX) res ]

  PrimOp _ op a -> do
    im <- compileAtom a
    pure $
      [ IMov (Reg EAX) im
      , compileOp op (Reg EAX)
      ]

  PrimBinOp _ op a1 a2 -> do
    im1 <- compileAtom a1
    im2 <- compileAtom a2
    pure $
      [ IMov (Reg EAX) im1 ]
      <> compileBinOp op im2


  Let _ addr binder body -> do
    asm <- concat <$> sequence
      [ compileExpr binder
      , pure [ IMov (RegOffset ESP addr) (Reg EAX) ]
      , compileExpr body
      ]
    popVar
    pure asm

  If path test true false -> do
   testAsm  <- compileExpr (Atom test)
   trueAsm  <- compileExpr true
   falseAsm <- compileExpr false
   pure $ concat
     [ testAsm
     , [ ICmp (Reg EAX) (Const falseValue)
       , IJe   ("if_false", path)
       , Label ("if_true",  path)
       ]
     , trueAsm
     , [ IJmp  ("if_done",  path)
       , Label ("if_false", path)
       ]
     , falseAsm
     ,  [ Label ("if_done", path) ]
     ]

-- | Compile an immediate value to an x86 argument
compileAtom :: Atom a -> CodeGen Arg
compileAtom = \case
  Num _ i
    | i > 1073741823 || i < -1073741824 ->
      throwErr $ "Integer overflow: " <> show i
    | otherwise ->
      pure $ Const (i `shiftL` 1)

  Bool _ b ->
    pure $ Const $ bool falseValue trueValue b

  Idn _ addr ->
    pure $ RegOffset ESP addr

-- | Compile a PrimOp and Arg to an x86 Instruction
compileOp :: PrimOp -> Arg -> Instruction
compileOp op_ arg = case op_ of
  NumOp op -> case op of
    Inc -> IAdd arg (Const $ shiftL 1 1)
    Dec -> ISub arg (Const $ shiftL 1 1)

  BoolOp op -> case op of
    Not -> IXor arg (Const boolTag)

-- | Compile a PrimBinOp and two Args,
--   where the first is in EAX and the other is passed to the function,
--   to an x86 Instruction.
--
--   The result will be in EAX.
--
compileBinOp :: PrimBinOp -> Arg -> [Instruction]
compileBinOp op_ arg2 = case op_ of
  NumBinOp op -> case op of
    Add -> [ IAdd (Reg EAX) arg2 ]
    Sub ->
      [ ISub (Reg EAX) arg2
      ]
    Mul ->
      [ IMul (Reg EAX) arg2
      , ISar (Reg EAX) (Const 1)
      ]

    Less ->
      [ ISar (Reg EAX) (Const 1)
      , IMov (Reg EBX) arg2
      , ISar (Reg EBX) (Const 1)
      , ISub (Reg EAX) (Reg EBX)
      , IAnd (Reg EAX) (Const trueValue)
      , IOr  (Reg EAX) (Const boolTag)
      ]

    Eq ->
      [ ISar (Reg EAX) (Const 1)
      , IMov (Reg EBX) arg2
      , ISar (Reg EBX) (Const 1)
      , IMov (Reg ECX) (Reg EAX)
      , ISub (Reg EAX) (Reg EBX)
      , ISub (Reg EBX) (Reg ECX)
      , IOr  (Reg EAX) (Reg EBX)
      , IAnd (Reg EAX) (Const trueValue)
      , IXor (Reg EAX) (Const trueValue)
      , IOr  (Reg EAX) (Const boolTag)
      ]

    Greater ->
      [ ISar (Reg EAX) (Const 1)
      , IMov (Reg EBX) arg2
      , ISar (Reg EBX) (Const 1)
      , ISub (Reg EBX) (Reg EAX)
      , IMov (Reg EAX) (Reg EBX)
      , IAnd (Reg EAX) (Const trueValue)
      , IOr  (Reg EAX) (Const boolTag)
      ]

    LessEq ->
      compileBinOp (NumBinOp Greater) arg2 ++
      [ IXor (Reg EAX) (Const trueValue)
      , IOr  (Reg EAX) (Const boolTag)
      ]

    GreaterEq ->
      compileBinOp (NumBinOp Less) arg2 ++
      [ IXor (Reg EAX) (Const trueValue)
      , IOr  (Reg EAX) (Const boolTag)
      ]

  BoolBinOp op -> case op of
    And ->
      [ IAnd (Reg EAX) arg2
      ]

    Or ->
      [ IOr (Reg EAX) arg2
      ]

    Xor ->
      [ IXor (Reg EAX) arg2
      , IOr  (Reg EAX) (Const boolTag)
      ]

------------------------
-- To Assembly String --
------------------------

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
  Label lbl ->
    ppLabel lbl <> ":"

-- | Pretty print a label
ppLabel :: Label -> String
ppLabel (lbl, i) =
  lbl <> "_" <> show i

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

-- | Pretty print a register
ppReg :: Reg -> String
ppReg = map toLower . show

---------------
-- Constants --
---------------

boolTag :: Int32
boolTag = 0x1

trueValue :: Int32
trueValue = -2147483647

falseValue :: Int32
falseValue = 0x1
