{- | Code generation for X86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, NegativeLiterals #-}

module Language.Nyanpasu.Assembly.X86.CodeGen where

import Language.Nyanpasu.Types
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

---------------------
-- Code Generation --
---------------------

-- | Compile an expression and output an assembly string
compileProgram :: AST.Expr () -> Either (Error ann) Assembly
compileProgram expr = do
  asmStr <- ppAsm <$> compileExprRaw expr
  pure $ Assembly $
    unlines
      [ prelude
      , asmStr
      , errors
      , suffix
      ]

  where
    prelude =
      unlines
        [ "section .text"
        , "global my_code"
        , "my_code:"
        ]

    suffix =
      unlines
        [ "end:"
        , "ret"
        ]

    errors = ppAsm (errorNotNumber ++ errorNotBool)

-- | Compile an expression and output a assembly list of instructions
compileExprRaw :: AST.Expr () -> Either (Error ann) [Instruction]
compileExprRaw expr = do
  e <- runExprToANF expr
  insts <- runExcept . flip evalStateT initState $ compileExpr e
  pure (withStackFrame e insts)


-- | Compile an expression to a list of instructions
compileExpr :: Expr Int32 -> CodeGen ann [Instruction]
compileExpr = \case
  Atom atom -> case atom of
    Idn _ addr -> do
      pure [ IMov (Reg EAX) (RegOffset EBP addr) ]

    _ -> do
      res <- compileAtom atom
      pure [ IMov (Reg EAX) res ]

  PrimOp _ op a -> do
    im <- compileAtom a
    pure $
        IMov (Reg EAX) im
      : compileOp op

  PrimBinOp _ op a1 a2 -> do
    im1 <- compileAtom a1
    im2 <- compileAtom a2
    pure $
      [ IMov (Reg EAX) im1 ]
      <> compileBinOp op im2


  Let _ addr binder body -> do
    asm <- concat <$> sequence
      [ compileExpr binder
      , pure [ IMov (RegOffset EBP addr) (Reg EAX) ]
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
       , IJe   ("if_false", Just path)
       , Label ("if_true",  Just path)
       ]
     , trueAsm
     , [ IJmp  ("if_done",  Just path)
       , Label ("if_false", Just path)
       ]
     , falseAsm
     ,  [ Label ("if_done", Just path) ]
     ]

-- | Compile an immediate value to an x86 argument
compileAtom :: Atom a -> CodeGen ann Arg
compileAtom = \case
  Num _ i
    | i > 1073741823 || i < -1073741824 ->
      throwErr $ "Integer overflow: " <> show i
    | otherwise ->
      pure $ Const (i `shiftL` 1)

  Bool _ b ->
    pure $ Const $ bool falseValue trueValue b

  Idn _ addr ->
    pure $ RegOffset EBP addr

-- | Compile a PrimOp to an x86 [Instruction]
compileOp :: PrimOp -> [Instruction]
compileOp = \case
  NumOp op ->
    checkNum (Reg EAX) ++
    case op of
      Inc -> [IAdd (Reg EAX) (Const $ shiftL 1 1)]
      Dec -> [ISub (Reg EAX) (Const $ shiftL 1 1)]

  BoolOp op ->
    checkBool (Reg EAX) ++
    case op of
      Not -> [IXor (Reg EAX) (Const trueTag)]

-- | Compile a PrimBinOp and two Args,
--   where the first is in EAX and the other is passed to the function,
--   to an x86 Instruction.
--
--   The result will be in EAX.
--
compileBinOp :: PrimBinOp -> Arg -> [Instruction]
compileBinOp op_ arg2 = case op_ of
  NumBinOp op -> checkNum (Reg EAX) ++ checkNum arg2 ++ case op of
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

    NotEq ->
      compileBinOp (NumBinOp Eq) arg2
      ++ compileOp (BoolOp Not)

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

  BoolBinOp op -> checkBool (Reg EAX) ++ checkBool arg2 ++ case op of
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

allocateStackFrame :: Expr Int32 -> [Instruction]
allocateStackFrame expr =
  [ IPush (Reg EBP)
  , IMov (Reg EBP) (Reg ESP)
  , ISub (Reg ESP) (ArgTimes 4 (Const $ countVars expr))
  ]

deallocateStackFrame :: [Instruction]
deallocateStackFrame =
  [ IMov (Reg ESP) (Reg EBP)
  , IPop (Reg EBP)
  , IRet
  ]

withStackFrame :: Expr Int32 -> [Instruction] -> [Instruction]
withStackFrame expr insts =
  allocateStackFrame expr ++ [EmptyInst] ++ insts ++  [EmptyInst] ++ deallocateStackFrame

errorNotNumber :: [Instruction]
errorNotNumber =
  [ Label ("error_not_number", Nothing)
  , IPush (Const 1)
  , ICall ("error", Nothing)
  ]

errorNotBool :: [Instruction]
errorNotBool =
  [ Label ("error_not_bool", Nothing)
  , IPush (Const 2)
  , ICall ("error", Nothing)
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

------------
-- Errors --
------------

checkNum :: Arg -> [Instruction]
checkNum arg =
  [ IPush arg
  , ITest arg (Const boolTag)
  , IJnz ("error_not_number", Nothing)
  , IAdd (Reg ESP) (ArgTimes 4 (Const 1))
  ]

checkBool :: Arg -> [Instruction]
checkBool arg =
  [ IPush arg
  , ITest arg (Const boolTag)
  , IJz ("error_not_bool", Nothing)
  , IAdd (Reg ESP) (ArgTimes 4 (Const 1))
  ]

---------------
-- Constants --
---------------

boolTag :: Int32
boolTag = 0x1

trueValue :: Int32
trueValue = -2147483647

falseValue :: Int32
falseValue = 0x1

trueTag :: Int32
trueTag = -2147483648

defLabelId :: Maybe Int32
defLabelId = Just (-1)
