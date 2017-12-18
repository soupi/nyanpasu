{- | Handle the cli arguments to the compiler

-}

module Language.Nyanpasu.Options
  ( parseArgs
  , Command(..)
  , Escape(..)
  )
where

import Data.Bool
import Data.Monoid
import Options.Applicative



parseArgs :: IO Command
parseArgs = execParser paramsParserInfo

-------------
-- Options --
-------------

data Command
  = CompileExpr
  | CompileProgram
  | CompileAndInterpret
  | CompileAndInterpretProgram
  | Interpret
  | AnfProgram
  | Samples (Maybe Escape)
  | SamplePrograms (Maybe Escape)
  deriving (Show, Read, Eq, Ord)

data Escape
  = Escape
  | EscapeHard
  deriving (Show, Read, Eq, Ord)

--------------------
-- Options Parser --
--------------------

paramsParserInfo :: ParserInfo Command
paramsParserInfo =
  info (helper <*> cmd) $
     fullDesc
  <> header "Nyanpasu"

cmd :: Parser Command
cmd =
  subparser
    ( command "compile" (info (pure CompileExpr <**> helper)
        ( progDesc "Compile an expression" ))

   <> command "cai" (info (pure CompileAndInterpret <**> helper)
        ( progDesc "Compile and interpret an expression" ))

   <> command "caip" (info (pure CompileAndInterpretProgram <**> helper)
        ( progDesc "Compile and interpret a program" ))

   <> command "interpret" (info (pure Interpret <**> helper)
        ( progDesc "Interpret an expression" ))

   <> command "samples" (info (Samples <$> esc <**> helper)
        ( progDesc "Pretty print samples" ))

   <> command "sample-programs" (info (SamplePrograms <$> esc <**> helper)
        ( progDesc "Pretty print program samples" ))

   <> command "compile-program" (info (pure CompileProgram <**> helper)
        ( progDesc "Compile a program" ))

   <> command "anf" (info (pure AnfProgram <**> helper)
        ( progDesc "A-normalize program" ))
    )

esc :: Parser (Maybe Escape)
esc =
    bool Nothing (Just Escape) <$>
      switch
      (long "escape"
       <> help "Escape output string"
      )
  <|>
    bool Nothing (Just EscapeHard) <$>
      switch
      (long "escape-hard"
       <> help "Escape output string twice"
      )
