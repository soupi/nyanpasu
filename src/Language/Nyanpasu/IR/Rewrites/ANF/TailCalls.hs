{- | Tail calls detection

-}


module Language.Nyanpasu.IR.Rewrites.ANF.TailCalls where

import Language.Nyanpasu.IR.ANF

addTailCalls :: Program a -> Program a
addTailCalls prog =
  prog
    { progDefs = addTailCallsToDef <$> progDefs prog
    }

addTailCallsToDef :: Def a -> Def a
addTailCallsToDef (Fun _a _lbl _args _expr) =
    Fun _a _lbl _args (go _expr)
  where
    callSpace = fromIntegral $ length _args
    go = \case
      If a cond e1 e2 ->
        If a cond (go e1) (go e2)

      Let a address arg body ->
        Let a address arg (go body)

      Call a l args ->
        TailCall a l callSpace args

      e -> e

