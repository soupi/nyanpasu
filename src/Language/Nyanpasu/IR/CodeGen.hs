{- | Code generation for X86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, NegativeLiterals, NamedFieldPuns #-}

module Language.Nyanpasu.IR.CodeGen where

import Language.Nyanpasu.Types
import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.ANF
import Language.Nyanpasu.IR.CodeGenUtils
import Language.Nyanpasu.IR.Rewrites
import Language.X86.Assembly hiding (ArithExpr(..), Label)
import Language.X86.PP (ppAsm)
import qualified Language.X86.Assembly as X86
import qualified Language.Nyanpasu.IR.AST as AST

import Data.Bits
import Data.Bool
import Data.Monoid ((<>))
import Text.Groom (groom)
import Control.Monad.State
import Control.Monad.Except

-- import Text.Groom

---------------------
-- Code Generation --
---------------------

newtype Assembly = Assembly String

-- | Compile an expression and output an assembly string
compileProgram :: AST.Program () -> Either Error Assembly
compileProgram program = do
  asmStr <- ppAsm <$> compileProgramRaw program
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
        , "extern error"
        , "extern print"
        ]

    suffix =
      unlines
        [ "end:"
        , "ret"
        ]

    errors = ppAsm . concat $
      [ errorNotNumber
      , errorNotBool
      , errorNotPair
      , errorNotEnoughMem
      ]

-- | Compile an expression and output a assembly list of instructions
compileProgramRawVM :: AST.Program () -> Either Error [Instruction]
compileProgramRawVM program = do
  prog'@Program{ progMain } <- rewrites program
  let funLabels = map (\lbl@(name, _) -> (name, lbl)) (defLabels prog')
  runExcept . flip evalStateT (initState funLabels) $ do
    funs <- mapM compileDef (progDefs prog')
    modify $ \CodeGenState{..} -> CodeGenState [] 1 cgNamer cgFunctions
    main <- compileExpr progMain
    let
      main' =
            lbl_ ("my_code", Nothing)
          : init (withStackFrame 0 progMain
            ( IMov (reg_ ESI) (lit_ 0) -- put heap pointer in ESI
            : main
            ))
    pure (concat funs ++ main')


-- | Compile an expression and output a assembly list of instructions
compileProgramRaw :: AST.Program () -> Either Error [Instruction]
compileProgramRaw program = do
  prog'@Program{ progMain } <- rewrites program
  let funLabels = map (\lbl@(name, _) -> (name, lbl)) (defLabels prog')
  runExcept . flip evalStateT (initState funLabels) $ do
    funs <- mapM compileDef (progDefs prog')
    modify $ \CodeGenState{..} -> CodeGenState [] 1 cgNamer cgFunctions
    main <- compileExpr progMain
    let
      main' =
            lbl_ ("my_code", Nothing)
          : withStackFrame 0 progMain
            ( IMov (reg_ ESI) (regOffset_ EBP (-2)) -- put heap pointer in ESI
            : main
            )
    pure (concat funs ++ main')


-- | Compile an expression to a list of instructions
compileExpr :: Expr Int32 -> CodeGen ann [Instruction]
compileExpr = \case
  Atom atom -> case atom of
    Idn _ addr -> do
      pure [ IMov (reg_ EAX) (regOffset_ EBP addr) ]

    _ -> do
      res <- compileAtom atom
      pure [ IMov (reg_ EAX) res ]

  MkPair _ a1 a2 -> do
    im1 <- compileAtom a1
    im2 <- compileAtom a2
    pure $
      [ ICmp (reg_ ESI) (lit_ heapSize)
      , IJge $ lblToAddr ("error_not_enough_memory", Nothing)

      , IMov (reg_ EBX) im1
      , IMov (reg_ ECX) im2

      , IMov (reg_ EAX) (reg_ ESI)
      , IAdd (reg_ EAX) (lit_ pairTag)

      , IMov (reg_ EBX) im1
      , IMov (Ref $ reg_ ESI) (reg_ EBX)
      , IAdd (reg_ ESI) (lit_ 4)

      , IMov (reg_ EBX) im2
      , IMov (Ref $ reg_ ESI) (reg_ EBX)
      , IAdd (reg_ ESI) (lit_ 4)
      ]

  PrimOp _ op a -> do
    im <- compileAtom a
    pure $
        IMov (reg_ EAX) im
      : compileOp op

  PrimBinOp _ op a1 a2 -> do
    im1 <- compileAtom a1
    im2 <- compileAtom a2
    pure $
      [ IMov (reg_ EAX) im1 ]
      <> compileBinOp op im2

  Let _ addr binder body -> do
    asm <- concat <$> sequence
      [ compileExpr binder
      , pure [ IMov (regOffset_ EBP addr) (reg_ EAX) ]
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
     , [ ICmp (reg_ EAX) (lit_ falseValue)
       , IJe $ lblToAddr ("if_false", Just path)
       , lbl_ ("if_true",  Just path)
       ]
     , trueAsm
     , [ IJmp  $ lblToAddr ("if_done",  Just path)
       , lbl_ ("if_false", Just path)
       ]
     , falseAsm
     ,  [ lbl_ ("if_done", Just path) ]
     ]

  CCall _ lbl args -> do
    pushArgs' <- mapM (pure . (\(setup, arg) -> setup ++ [IPush arg]) . compileArg <=< compileAtom) args
    -- in this calling convention we push arguments in reverse
    pure $ concat (reverse pushArgs') ++ [ICall $ lblToAddr lbl, ISub (reg_ ESP) (times4_ $ fromIntegral $ length args)]

  Call _ lbl args -> do
    pushArgs' <- mapM (pure . (\(setup, arg) -> setup ++ [IPush arg]) . compileArg <=< compileAtom) args
    -- in this calling convention we push arguments in reverse
    pure $ concat (reverse pushArgs') ++ [ICall $ lblToAddr lbl]

  TailCall _ lbl argsSpace args -> do
    let argsSize = fromIntegral (length args)
    pushArgs' <- mapM (pure . (\(setup, arg) -> setup ++ [IPush arg]) . compileArg <=< compileAtom) args
    pure $
         [ISub (reg_ ESP) (times4_ $ argsSize - argsSpace) | argsSize > argsSpace]                -- padding
      ++ [IMov (reg_ EDX) (regOffset_ EBP 0), IPush (reg_ EDX)]                                   -- save saved EBP
      ++ [IMov (reg_ EDX) (regOffset_ EBP (-1)), IPush (reg_ EDX)]                                -- save saved return address
      ++ concat pushArgs'                                                                         -- push arguments
      ++ [IMov (reg_ ECX) (reg_ EBP), IAdd (reg_ ECX) (times4_ $ 1 + argsSpace)]                  -- point to start of arguments
      ++ concatMap (\i -> [IPop (reg_ EAX), IMov (regOffset_ ECX i) (reg_ EAX)]) [0..argsSize]    -- setup args
      ++ [IPop (reg_ EAX), IMov (regOffset_ ECX (argsSize + 1)) (reg_ EAX)]                       -- save old return address
      ++ [ISub (reg_ ECX) (times4_ $ 1 + argsSize), IMov (reg_ ESP) (reg_ ECX)]                   -- set ESP to point to current EBP
      ++ [IPop (reg_ EBP)]                                                                        -- set EBP to old EBP
      ++ [IJmp $ lblToAddr lbl]                                                                   -- jump to function

-- | Compile an immediate value to an x86 argument
compileAtom :: Atom a -> CodeGen ann Arg
compileAtom = \case
  Num _ i
    | i > 1073741823 || i < -1073741824 ->
      throwErr $ "Integer overflow: " <> show i
    | otherwise ->
      pure $ lit_ (i `shiftL` 1)

  Bool _ b ->
    pure $ lit_ $ bool falseValue trueValue b

  Idn _ addr ->
    pure $ regOffset_ EBP addr

-- | Compile a PrimOp to an x86 [Instruction]
compileOp :: PrimOp -> [Instruction]
compileOp = \case
  NumOp op ->
    checkNum (reg_ EAX) ++
    case op of
      Inc -> [IAdd (reg_ EAX) (lit_ $ shiftL 1 1)]
      Dec -> [ISub (reg_ EAX) (lit_ $ shiftL 1 1)]

  BoolOp op ->
    checkBool (reg_ EAX) ++
    case op of
      Not -> [IXor (reg_ EAX) (lit_ trueTag)]

  PairOp op ->
    checkPair (reg_ EAX) ++ [ISub (reg_ EAX) (lit_ pairTag)] ++
    case op of
      First ->
        [ IMov (reg_ EAX) (Ref $ reg_ EAX)
        ]
      Second ->
        [ IAdd (reg_ EAX) (lit_ 4)
        , IMov (reg_ EAX) (Ref $ reg_ EAX)
        ]

-- | Compile a PrimBinOp and two Args,
--   where the first is in EAX and the other is passed to the function,
--   to an x86 Instruction.
--
--   The result will be in EAX.
--
compileBinOp :: PrimBinOp -> Arg -> [Instruction]
compileBinOp op_ arg2@(compileArg -> (setup, arg2')) = case op_ of
  NumBinOp op -> checkNum (reg_ EAX) ++ checkNum arg2 ++ case op of
    Add ->
      setup ++
      [ IAdd (reg_ EAX) arg2'
      ]
    Sub ->
      setup ++
      [ ISub (reg_ EAX) arg2'
      ]
    Mul ->
      setup ++
      [ IMul arg2'
      , ISar (reg_ EAX) (lit_ 1)
      ]

    Less ->
      [ ISar (reg_ EAX) (lit_ 1)
      , IMov (reg_ EBX) arg2
      , ISar (reg_ EBX) (lit_ 1)
      , ISub (reg_ EAX) (reg_ EBX)
      , IAnd (reg_ EAX) (lit_ trueValue)
      , IOr  (reg_ EAX) (lit_ boolTag)
      ]

    Eq ->
      [ ISar (reg_ EAX) (lit_ 1)
      , IMov (reg_ EBX) arg2
      , ISar (reg_ EBX) (lit_ 1)
      , IMov (reg_ ECX) (reg_ EAX)
      , ISub (reg_ EAX) (reg_ EBX)
      , ISub (reg_ EBX) (reg_ ECX)
      , IOr  (reg_ EAX) (reg_ EBX)
      , IAnd (reg_ EAX) (lit_ trueValue)
      , IXor (reg_ EAX) (lit_ trueValue)
      , IOr  (reg_ EAX) (lit_ boolTag)
      ]

    NotEq ->
      compileBinOp (NumBinOp Eq) arg2
      ++ compileOp (BoolOp Not)

    Greater ->
      [ ISar (reg_ EAX) (lit_ 1)
      , IMov (reg_ EBX) arg2
      , ISar (reg_ EBX) (lit_ 1)
      , ISub (reg_ EBX) (reg_ EAX)
      , IMov (reg_ EAX) (reg_ EBX)
      , IAnd (reg_ EAX) (lit_ trueValue)
      , IOr  (reg_ EAX) (lit_ boolTag)
      ]

    LessEq ->
      compileBinOp (NumBinOp Greater) arg2 ++
      [ IXor (reg_ EAX) (lit_ trueValue)
      , IOr  (reg_ EAX) (lit_ boolTag)
      ]

    GreaterEq ->
      compileBinOp (NumBinOp Less) arg2 ++
      [ IXor (reg_ EAX) (lit_ trueValue)
      , IOr  (reg_ EAX) (lit_ boolTag)
      ]

  BoolBinOp op -> checkBool (reg_ EAX) ++ checkBool arg2 ++ case op of
    And ->
      [ IAnd (reg_ EAX) arg2
      ]

    Or ->
      [ IOr (reg_ EAX) arg2
      ]

    Xor ->
      [ IXor (reg_ EAX) arg2
      , IOr  (reg_ EAX) (lit_ boolTag)
      ]


-- | Compile a function definition
compileDef :: Def Int32 -> CodeGen ann [Instruction]
compileDef (Fun _ lbl args body) = do
  modify $ \CodeGenState{..} ->
    let env = zip args [(-2),(-3)..]
    in CodeGenState env 1 cgNamer cgFunctions
  (lbl_ lbl :) . withStackFrame (fromIntegral $ length args) body <$> compileExpr body

allocateStackFrame :: Expr Int32 -> [Instruction]
allocateStackFrame expr =
  [ IPush (reg_ EBP)
  , IMov (reg_ EBP) (reg_ ESP)
  , ISub (reg_ ESP) (times4_ $ countVars expr)
  ]

deallocateStackFrame :: Int32 -> [Instruction]
deallocateStackFrame argsSpace =
  [ IMov (reg_ ESP) (reg_ EBP)
  , IPop (reg_ EBP)
  , IPop (reg_ EDX)                      -- save return address
  , IAdd (reg_ ESP) (times4_ argsSpace)  -- remove arguments
  , IPush (reg_ EDX)                     -- push return address for Ret to work
  , IRet
  ]

withStackFrame :: Int32 -> Expr Int32 -> [Instruction] -> [Instruction]
withStackFrame argsSpace expr insts =
  allocateStackFrame expr ++ insts ++ deallocateStackFrame argsSpace

------------
-- Errors --
------------

errorNotNumber :: [Instruction]
errorNotNumber =
  [ X86.Label "error_not_number"
  , IPush (lit_ 1)
  , ICall $ lblToAddr ("error", Nothing)
  ]

errorNotBool :: [Instruction]
errorNotBool =
  [ X86.Label "error_not_bool"
  , IPush (lit_ 2)
  , ICall $ lblToAddr ("error", Nothing)
  ]

errorNotPair :: [Instruction]
errorNotPair =
  [ X86.Label "error_not_pair"
  , IPush (lit_ 3)
  , ICall $ lblToAddr ("error", Nothing)
  ]

errorNotEnoughMem :: [Instruction]
errorNotEnoughMem =
  [ X86.Label "error_not_enough_memory"
  , IPush (reg_ ESI)
  , IPush (lit_ 17)
  , ICall $ lblToAddr ("error", Nothing)
  ]

checkNum :: Arg -> [Instruction]
checkNum = \case
  AE (X86.Lit i) | i `mod` 2 == 0 -> []
  AE (X86.Lit other) ->
    [ IPush (AE $ X86.Lit other)
    , IJmp $ lblToAddr ("error_not_number", Nothing)
    ]
  (compileArg -> (setup, arg)) ->
    setup ++
    [ IPush arg
    , ITest arg (AE $ X86.Lit boolTag)
    , IJnz $ lblToAddr ("error_not_number", Nothing)
    , IAdd (reg_ ESP) (times4_ 1)
    ]

checkBool :: Arg -> [Instruction]
checkBool = \case
  AE (X86.Lit b) | b `elem` [trueValue, falseValue] -> []
  AE (X86.Lit other) ->
    [ IPush (AE $ X86.Lit other)
    , IJmp $ lblToAddr ("error_not_bool", Nothing)
    ]
  (compileArg -> (setup, arg)) ->
    setup ++
    [ IPush arg
    , ITest arg (lit_ boolTag)
    , IJz $ lblToAddr ("error_not_bool", Nothing)
    , IAdd (reg_ ESP) (times4_ 1)
    ]

checkPair :: Arg -> [Instruction]
checkPair = \case
  AE (X86.Lit b) | b `mod` 8 == 7 -> []
  AE (X86.Lit other) ->
    [ IPush (lit_ other)
    , IJmp $ lblToAddr ("error_not_pair", Nothing)
    ]
  (compileArg -> (setup, arg)) ->
    setup ++
    [ IPush arg
    , ITest arg (lit_ pairTag)
    , IJz $ lblToAddr ("error_not_pair", Nothing)
    , IAdd (reg_ ESP) (times4_ 1)
    ]

compileArg :: Arg -> ([Instruction], Arg)
compileArg arg = case arg of
  Ref{} -> (, reg_ EDX)
    [ IMov (reg_ EDX) arg
    ]
  _ ->
    ([], arg)

-- Utils --

reg_ :: Reg -> Arg
reg_ = AE . X86.Var

lblToAddr :: Label -> Address
lblToAddr = X86.Var . AL . fromLabel . lbl_
  where
    fromLabel (X86.Label l) = l
    fromLabel x =
      error $ "used after `lbl_` which is expected to return a Label, but it returned: " ++ groom x

lbl_ :: Label -> Instruction
lbl_ (s, m) = X86.Label $
  case m of
    Just i  -> s ++ "__" ++ show i
    Nothing -> s

lit_ :: Int32 -> Arg
lit_ = AE . X86.Lit

regOffset_ :: Reg -> Int32 -> Arg
regOffset_ reg offset =
  Ref
    $ AE
    $ X86.Sub (X86.Var reg)
    $ X86.Mul (X86.Lit 4) (X86.Lit offset)

times4_ :: Int32 -> Arg
times4_ = AE . X86.Mul (X86.Lit 4) . X86.Lit

---------------
-- Constants --
---------------


pairTag :: Int32
pairTag = 7

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

heapSize :: Int32
heapSize = 1024*1024

printReg :: Reg -> [Instruction]
printReg reg =
  [ IPush (reg_ reg)
  , ICall $ lblToAddr ("print", Nothing)
  , IPop (reg_ reg)
  ]
