{- | An interpreter for x86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86.Interpreter where

import Data.Bits
import Data.Maybe
import Control.Monad.Except
import qualified Data.Map as M

import Language.Nyanpasu.Types
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST (Expr)
import Language.Nyanpasu.LL.CodeGenUtils
import Language.Nyanpasu.Assembly.X86.CodeGen


-- | Representation of the machine state
data Machine = Machine
  { stack :: M.Map Int32 Int32
  , regs :: M.Map Reg Int32
  , zf :: Bool
  }
  deriving (Show, Read, Eq, Ord)

-- | An instruction tree
--
--   Each Label points to the list of instructions that starts from it
--
--   A jmp instruction will lookup the label in the Map
--
type Instructions
  = M.Map Label [Instruction]


-- | Initial Instructions

initInsts :: Instructions
initInsts = M.fromList
  [ ( ("end", -1)
    , [IRet]
    )
  , ( ("error_not_bool",-1)
    , [IRet]
    )
  , ( ("error_not_number",-1)
    , [IRet]
    )
  ]

-- | Create an instructions tree from a list of Instructions
--
--   May fail if a label is redefined
--
mkInstructions :: [Instruction] -> Either (Error ann) Instructions
mkInstructions = go initInsts ("start", -1) []
  where
    go :: Instructions -> Label -> [Instruction] -> [Instruction] -> Either (Error ann) Instructions
    go insts lbl lblInsts = \case
      [] -> pure $ M.insert lbl (reverse lblInsts) insts

      Label newLbl : rest -> do
        case M.lookup newLbl insts of
          Nothing -> do
            go (M.insert lbl (reverse (IJmp newLbl : lblInsts)) insts) newLbl [] rest
          Just _ ->
            throwErr $ "Label '" ++ ppLabel lbl ++ "' redefinition."

      inst : rest ->
        go insts lbl (inst : lblInsts) rest

initMachine :: Machine
initMachine = Machine M.empty M.empty False

-- | Compile and interpret an AST.Expr
interpret :: Expr () -> Either (Error ann) Int32
interpret = runInterpreter <=< compileExprRaw

-- | Execute instructions
runInterpreter :: [Instruction] -> Either (Error ann) Int32
runInterpreter instructions = do
  insts <- mkInstructions instructions
  lookupErr EAX . regs =<<
   interpreterStep initMachine insts =<<
     lookupErr ("start", -1) insts

-- | One step of instructions
interpreterStep :: Machine -> Instructions -> [Instruction] -> Either (Error ann) Machine
interpreterStep m insts = \case
  [] -> pure m
  inst : next -> do
    case inst of
      Label l ->
        throwErr $ "Labels are not supposed to exist in this stage: " ++ ppLabel l

      IMov a1 a2 -> binModSrc (pure .* const id) a1 a2
      IAdd a1 a2 -> binModSrc (pure .* (+)) a1 a2
      ISub a1 a2 -> binModSrc (pure .* (-)) a1 a2
      IMul a1 a2 -> binModSrc (pure .* (*)) a1 a2
      IAnd a1 a2 -> binModSrc (pure .* (.&.)) a1 a2
      IXor a1 a2 -> binModSrc (pure .* xor) a1 a2
      IOr  a1 a2 -> binModSrc (pure .* (.|.)) a1 a2
      ISal a1 a2 -> binModSrc (pure .* \x y -> shiftL x (fromIntegral y)) a1 a2
      ISar a1 a2 -> binModSrc (pure .* \x y -> shiftR x (fromIntegral y)) a1 a2
      IShl a1 a2 ->
        binModSrc
          (pure .* \x y ->
            shiftL (clearBit x 31) (fromIntegral y)
          )
          a1
          a2

      IShr a1 a2 ->
        binModSrc
          (pure .* \x y ->
            clearBit (shiftR (clearBit x 31) (fromIntegral y)) 31
          )
          a1
          a2

      ICmp a1 a2 -> do
        a1' <- getDest m a1
        a2' <- getDest m a2
        interpreterStep (m { zf = a1' == a2' }) insts next

      IJmp lbl ->
        interpreterStep m insts =<<
          lookupErr lbl insts

      IJe lbl ->
        interpreterStep m insts =<<
          if zf m
            then
              lookupErr lbl insts
            else
              pure next

      IJnz lbl ->
        interpreterStep m insts =<<
          if zf m
            then
              pure next
            else
              lookupErr lbl insts

      IRet -> pure m

    where
      binModSrc f a1 a2 = do
        toSrc <- getSrcF f m a1
        dest  <- getDest m a2
        newMachine <- toSrc dest
        interpreterStep newMachine insts next

getSrcF :: MonadError (Error ann) m => (Int32 -> Int32 -> m Int32) -> Machine -> Arg -> m (Int32 -> m Machine)
getSrcF f m = \case
  Reg r -> do
    rv <- getDest m (Reg r)
    pure $ \i -> do
      result <- f rv i
      pure $ m { regs = M.insert r result (regs m) }

  RegOffset ESP n -> do
    rv <- getDest m (RegOffset ESP n) `catchError` \_ -> pure 0
    pure $ \i -> do
      result <- f rv i
      pure $ m { stack = M.insert n result (stack m) }

  x ->
    throwErr $ "No supported: " ++ show x

getDest :: MonadError (Error ann) m => Machine -> Arg -> m Int32
getDest m = \case
  Reg r ->
    pure . fromMaybe 0 $ M.lookup r (regs m)

  RegOffset ESP n ->
    lookupErr n (stack m)

  Const n ->
    pure n

  x ->
    throwErr $ "No supported: " ++ show x
