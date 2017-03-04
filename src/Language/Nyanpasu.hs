module Language.Nyanpasu
  ( module Export
  , run
  )
where

import Language.Nyanpasu.LL as Export

run :: IO ()
run = do
  putStrLn (compileProgram sample)

sample :: Expr
sample = Inc $ Dec $ Dec $ Inc $ Dec $ Inc $ Inc $ Num 7
