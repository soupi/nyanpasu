{- | Definition of the low-level IR for the compiler
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86 where

import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.ANF
import Language.Nyanpasu.LL.CodeGenUtils
import qualified Language.Nyanpasu.LL.AST as AST

import Data.Data
import GHC.Generics
import Control.DeepSeq

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

newtype Assembly = Assembly String
  deriving (Eq, Ord, Generic, NFData, Data, Typeable)

instance Show Assembly where
  show (Assembly asmStr) = asmStr

---------------------
-- Code Generation --
---------------------

compileProgram :: AST.Expr () -> Either Error Assembly
compileProgram expr = do
  e <- {- (\e -> trace (groom e) e) <$> -}
    runExprToANF expr
  runExcept . flip evalStateT initState $ do
    compiled <- compileExpr e
    asmStr   <- ppAsm compiled
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


compileExpr :: Expr Int -> CodeGen [Instruction]
compileExpr = \case
  Atom atom -> case atom of
    Num _ i ->
      pure [ IMov (Reg EAX) (Const i) ]

    Idn _ addr -> do
      pure [ IMov (Reg EAX) (RegOffset ESP addr) ]


  PrimOp _ op a -> do
    pure $
      [ IMov (Reg EAX) (compileAtom a)
      , compileOp op (Reg EAX)
      ]

  PrimBinOp _ op a1 a2 -> do
    pure $
      [ IMov (Reg EAX) (compileAtom a1)
      , compileBinOp op (Reg EAX) (compileAtom a2)
      ]


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
   
compileAtom :: Atom a -> Arg
compileAtom = \case
  Num _ i ->
    Const i
  Idn _ addr ->
    RegOffset ESP addr

compileOp :: PrimOp -> Arg -> Instruction
compileOp op arg = case op of
  Inc -> IAdd arg (Const 1)
  Dec -> ISub arg (Const 1)

compileBinOp :: PrimBinOp -> Arg -> Arg -> Instruction
compileBinOp op arg1 arg2 = case op of
  Add -> IAdd arg1 arg2
  Sub -> ISub arg1 arg2

------------------------
-- To Assembly String --
------------------------

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

