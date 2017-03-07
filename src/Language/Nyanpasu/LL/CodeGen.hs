module Language.Nyanpasu.LL.CodeGen where

import Language.Nyanpasu.LL.AST
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error

import Data.Char (toLower)
import Data.Monoid
import Control.Monad.State
import Control.Monad.Except

data CodeGenState = CodeGenState
  { cgSymbols :: Env
  , cgCounter :: !Address
  }
  deriving (Show, Read, Eq, Ord)

initState :: CodeGenState
initState = CodeGenState [] 1

type Address = Int

type Env = [(Name, Address)]

type CodeGen a
  = StateT CodeGenState (Except Error) a

insertVar :: String -> CodeGen Address
insertVar name = do
  CodeGenState env addr <- get
  put (CodeGenState ((name, addr) : env) (addr + 1))
  pure addr

popVar :: CodeGen ()
popVar = do
  CodeGenState env addr <- get
  put (CodeGenState (tail env) (addr - 1))

compileProgram :: Expr a -> Either Error String
compileProgram e = runExcept . flip evalStateT initState $ do
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


compileExpr :: Expr a -> CodeGen [Instruction]
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

