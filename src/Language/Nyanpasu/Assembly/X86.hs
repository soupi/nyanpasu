{- | Re-exporter for X86 sub-namespace
-}

module Language.Nyanpasu.Assembly.X86
  ( module Export
  )
where

import Language.Nyanpasu.Assembly.X86.CodeGen as Export
import Language.Nyanpasu.Assembly.X86.Interpreter as Export
