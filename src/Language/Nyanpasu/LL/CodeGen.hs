module Language.Nyanpasu.LL.CodeGen where

import Language.Nyanpasu.LL.AST

import Data.Char (toLower)
import Data.Monoid


compileProgram :: Expr -> String
compileProgram e =
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
    asmStr = (ppAsm . compileExpr) e
    suffix = "ret"


compileExpr :: Expr -> [Instruction]
compileExpr e =
  [ IMov (Reg EAX) (Const e) ]

ppAsm :: [Instruction] -> String
ppAsm = unlines . map ppInstruction

ppInstruction :: Instruction -> String
ppInstruction = \case
  IMov dest src ->
    unwords
      [ "mov"
      , ppArg dest <> ","
      , ppArg src
      ]

ppArg :: Arg -> String
ppArg = \case
  Const i -> show i
  Reg r   -> ppReg r

ppReg :: Reg -> String
ppReg = map toLower . show

