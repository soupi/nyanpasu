{- | An interpreter for x86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86.Interpreter where

import Data.Maybe
import Control.Monad.Except
import qualified Data.Map as M

import Language.Nyanpasu.LL.AST (Expr)
import Language.Nyanpasu.Assembly.X86.CodeGen
import Language.Nyanpasu.Error
import Language.Nyanpasu.Utils


-- | Representation of the machine state
data Machine = Machine
  { stack :: M.Map Int Int
  , regs :: M.Map Reg Int
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


-- | Create an instructions tree from a list of Instructions
--
--   May fail if a label is redefined
--
mkInstructions :: [Instruction] -> Either Error Instructions
mkInstructions = go M.empty ("start", -1) []
  where
    go :: Instructions -> Label -> [Instruction] -> [Instruction] -> Either Error Instructions
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
interpret :: Expr () -> Either Error Int
interpret = runInterpreter <=< compileExprRaw

-- | Execute instructions
runInterpreter :: [Instruction] -> Either Error Int
runInterpreter instructions = do
  insts <- mkInstructions instructions
  lookupErr EAX . regs =<<
   interpreterStep initMachine insts =<<
     lookupErr ("start", -1) insts

-- | One step of instructions
interpreterStep :: Machine -> Instructions -> [Instruction] -> Either Error Machine
interpreterStep m insts = \case
  [] -> pure m
  inst : next -> do
    case inst of
      Label l ->
        throwErr $ "Labels are not supposed to exist in this stage: " ++ ppLabel l

      IMov a1 a2 -> do
        toSrc <- getSrcF (const id) m a1
        dest  <- getDest m a2
        interpreterStep (toSrc dest) insts next

      IAdd a1 a2 -> do
        toSrc <- getSrcF (+) m a1
        dest  <- getDest m a2
        interpreterStep (toSrc dest) insts next

      ISub a1 a2 -> do
        toSrc <- getSrcF (-) m a1
        dest  <- getDest m a2
        interpreterStep (toSrc dest) insts next

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


getSrcF :: MonadError Error f => (Int -> Int -> Int) -> Machine -> Arg -> f (Int -> Machine)
getSrcF f m = \case
  Reg r -> do
    rv <- getDest m (Reg r)
    pure $ \i -> m { regs = M.insert r (f rv i) (regs m) }

  RegOffset ESP n -> do
    rv <- getDest m (RegOffset ESP n) `catchError` \_ -> pure 0
    pure $ \i -> m { stack = M.insert n (f rv i) (stack m) }

  x ->
    throwErr $ "No supported: " ++ show x

getDest :: MonadError Error f => Machine -> Arg -> f Int
getDest m = \case
  Reg r ->
    pure . fromMaybe 0 $ M.lookup r (regs m)

  RegOffset ESP n ->
    lookupErr n (stack m)

  Const n ->
    pure n

  x ->
    throwErr $ "No supported: " ++ show x
