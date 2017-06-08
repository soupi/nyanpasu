{- | An interpreter for x86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86.Interpreter where

import Data.Monoid
import Data.Bits
import Data.Maybe
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Vector as V

import Language.Nyanpasu.Types
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.LL.AST (Expr)
import Language.Nyanpasu.Assembly.X86.CodeGen

import Text.Groom

-- | Representation of the machine state
data Machine = Machine
  { stack :: V.Vector Int32
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
mkInstructions = go initInsts ("start", -1) [] . rewrites
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

rewrites :: [Instruction] -> [Instruction]
rewrites = concatMap $ \case
  IPush arg ->
    [ IMov (RegOffset ESP 0) arg
    , ISub (Reg ESP) (Const 1)
    ]

  IPop arg ->
    [ IMov arg (RegOffset ESP 0)
    , IAdd (Reg ESP) (Const 1)
    ]

  ICall lbl ->
    [ 
    ]

  i -> [ i ]

initMachine :: Machine
initMachine = Machine (V.replicate 1000 0) (M.fromList [(ESP, 900)]) False

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
        a1' <- getSrc m a1
        a2' <- getSrc m a2
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

      _ -> throwErr $ "Unexpected instruction: " <> groom inst

    where
      binModSrc f a1 a2 = do
        toDest <- modifyDest f m a1
        src    <- getSrc m a2
        newMachine <- toDest src
        interpreterStep newMachine insts next

modifyDest :: MonadError (Error ann) m => (Int32 -> Int32 -> m Int32) -> Machine -> Arg -> m (Int32 -> m Machine)
modifyDest f m = \case
  Reg r -> do
    rv <- getSrc m (Reg r)
    pure $ \i -> do
      result <- f rv i
      pure $ m { regs = M.insert r result (regs m) }

  RegOffset reg n -> do
    regVal <- getSrc m (Reg reg) `catchError` \_ -> pure 0
    regOffsetVal <- getSrc m (RegOffset reg n) `catchError` \_ -> pure 0
    pure $ \i -> do
      result <- f regOffsetVal i
      let index = fromIntegral (regVal - n)
      pure $ m { stack = stack m V.// [(index, result)] }

  x ->
    throwErr $ "getSrcF: Not supported: " ++ show x

getSrc :: MonadError (Error ann) m => Machine -> Arg -> m Int32
getSrc m = \case
  Reg r ->
    pure . fromMaybe 0 $ M.lookup r (regs m)

  RegOffset reg n -> do
    let rv = fromMaybe 0 $ M.lookup reg (regs m)
    let index = fromIntegral (rv - n)
    pure $ (stack m V.! index)

  Const n ->
    pure n

  ArgTimes _ arg ->
    getSrc m arg

  x ->
    throwErr $ "getDest: Not supported: " ++ show x
