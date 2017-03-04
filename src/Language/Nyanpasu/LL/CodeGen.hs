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
compileExpr = reverse . go
  where
    go = \case
      Num i ->
        [ IMov (Reg EAX) (Const i) ]
      Inc e ->
          IAdd (Reg EAX) (Const 1)
        : go e
      Dec e ->
          ISub (Reg EAX) (Const 1)
        : go e

ppAsm :: [Instruction] -> String
ppAsm = unlines . map ppInstruction

ppInstruction :: Instruction -> String
ppInstruction = \case
  IMov dest src ->
    ppOp "mov" dest src
  IAdd dest src ->
    ppOp "add" dest src
  ISub dest src ->
    ppOp "sub" dest src

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

ppReg :: Reg -> String
ppReg = map toLower . show

