{-# LANGUAGE MultiWayIf #-}
{- | An interpreter for x86
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable #-}

module Language.Nyanpasu.Assembly.X86.Interpreter where

import Data.Monoid
import Data.Bits
import Data.Maybe
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Vector as V

import Language.Nyanpasu.Types
import Language.Nyanpasu.Utils
import Language.Nyanpasu.Error
import Language.Nyanpasu.IR.Interpreter (int32ToVal)
import Language.Nyanpasu.Assembly.X86

import Text.Groom
import Debug.Trace

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
  [ ( ("end", Nothing)
    , [IRet]
    )
  , ( ("error_not_bool", Nothing)
    , [ IPush (Const 2)
      , ICall ("error", Nothing)
      ]
    )
  , ( ("error_not_number", Nothing)
    , [ IPush (Const 1)
      , ICall ("error", Nothing)
      ]
    )
  , ( ("print", Nothing)
    , [ ICall ("print", Nothing)
      ]
    )
  , ( ("call", defLabelId)
    , []
    )
  ]

-- | Initial machine state
initMachine :: Machine
initMachine = Machine (V.replicate 1000 0 V.// [(901, -1)]) (M.fromList [(ESP, 900)]) False


-- | Create an instructions tree from a list of Instructions
--
--   May fail if a label is redefined
--
mkInstructions :: [Instruction] -> Either Error Instructions
mkInstructions = go (fmap rewrites initInsts) ("start", Nothing) [] . rewrites
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

biggestLabel :: [Instruction] -> Int32
biggestLabel insts = (maximum . (0 :)) . flip map insts $ \case
  Label (_, Just i) -> i
  _ -> 0

-- | rewrite some assembly instructions to simpler instructions
rewrites :: [Instruction] -> [Instruction]
rewrites insts = evalState (go insts) (biggestLabel insts :: Int32)
  where
    go = fmap (fmap concat) . mapM $ \case
      IPush arg -> pure
        [ IMov (RegOffset ESP 0) arg
        , ISub (Reg ESP) (Const 1)
        ]

      IPop arg -> pure
        [ IAdd (Reg ESP) (Const 1)
        , IMov arg (RegOffset ESP 0)
        ]

      ICall lbl | lbl /= ("error", Nothing) -> do
        i <- newLabel
        go
          [ IPush (Const i)
          , Label ("call", Just i)
          , IJmp lbl
          ]

      EmptyInst -> pure []

      i -> pure [ i ]

      where
        newLabel = do
          i <- get
          put (i + 1)
          pure i

-- | Execute instructions
runInterpreter :: [Instruction] -> Either Error Int32
runInterpreter instructions = do
  insts <- mkInstructions instructions
  lookupErr EAX . regs =<<
   interpreterStep initMachine insts =<<
     lookupErr ("start", Nothing) insts

-- | One step of instructions
interpreterStep :: Machine -> Instructions -> [Instruction] -> Either Error Machine
interpreterStep m insts = \case
  [] -> pure m
  inst : next -> do
    -- case snd $ traceL "regs,inst" (regs m, inst) of
    case inst of
      Label l ->
        throwErr $ "Labels are not supposed to exist in this stage: " ++ ppLabel l

      IMov a1 a2 -> binModSrc (pure .* const id) a1 a2
      IAdd a1 a2 -> binModSrc (pure .* (+)) a1 a2
      ISub a1 a2 -> binModSrc (pure .* (-)) a1 a2
      IMul a1    -> binModSrc (pure .* (*)) (Reg EAX) a1
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

      ITest a1 a2 -> do
        a1' <- getSrc m a1
        a2' <- getSrc m a2
        interpreterStep (m { zf = a1' .&. a2' == 0 }) insts next

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

      IJz lbl ->
        interpreterStep m insts =<<
          if zf m
            then
              lookupErr lbl insts
            else
              pure next

      IRet -> do
        rv <- maybe (throwErr "Could not find variable ESP") (pure . (+1)) $ M.lookup ESP (regs m)
        let retAddr = (stack m V.! fromIntegral rv)
        let m' = m { regs = M.insert ESP rv (regs m) }
        interpreterStep m' insts (IJmp ("call", Just retAddr) : next)

      ICall ("error", Nothing) -> do
        rv <- maybe (throwErr "Could not find variable ESP") (pure . (+2)) $ M.lookup ESP (regs m)
        let errCode = (stack m V.! fromIntegral (rv - 1))
        let val = (stack m V.! fromIntegral rv)
        throwErr $ case int32ToVal () val of
          Right v
            | errCode == 1 -> "Type Error: Expected a number but got: " ++ show v
            | errCode == 2 -> "Type Error: Expected a boolean but got: " ++ show v
          _ -> unlines $
            [ "Unknown arguments to error: errCode = " ++ show errCode ++ ", val = " ++ show val
            , show m
            ]

      _ -> throwErr $ "Unexpected instruction: " <> groom inst

      ICall ("print", Nothing) -> do
        rv <- maybe (throwErr "Could not find variable ESP") (pure . (+1)) $ M.lookup ESP (regs m)
        let val = (stack m V.! fromIntegral rv)
        traceShowId val `seq` interpreterStep m insts (IMov (Reg EAX) (Const val) : IRet : next)

    where
      binModSrc f a1 a2 = do
        toDest <- modifyDest f m a1
        src    <- getSrc m a2
        newMachine <- toDest src
        interpreterStep newMachine insts next

modifyDest :: MonadError Error m => (Int32 -> Int32 -> m Int32) -> Machine -> Arg -> m (Int32 -> m Machine)
modifyDest f m = \case
  Reg r -> do
    rv <- getSrc m (Reg r)
    pure $ \i -> do
      result <- f rv i
      pure $ m { zf = result == 0, regs = M.insert r result (regs m) }

  RegOffset reg n -> do
    regVal <- getSrc m (Reg reg) `catchError` \_ -> pure 0
    regOffsetVal <- getSrc m (RegOffset reg n) `catchError` \_ -> pure 0
    pure $ \i -> do
      result <- f regOffsetVal i
      let index = fromIntegral (regVal - n)
      pure $ m { zf = result == 0, stack = stack m V.// [(index, result)] }

  x ->
    throwErr $ "getSrcF: Not supported: " ++ show x

getSrc :: MonadError Error m => Machine -> Arg -> m Int32
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

  -- x ->
  --   throwErr $ "getDest: Not supported: " ++ show x

defLabelId :: Maybe Int32
defLabelId = Just (-1)
