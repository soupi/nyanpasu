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
  = Compile
  | CompileAndInterpret
  | Interpret
  | Samples (Maybe Escape)
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
    ( command "compile" (info (pure Compile <**> helper)
        ( progDesc "Compile an expression" ))

   <> command "cai" (info (pure CompileAndInterpret <**> helper)
        ( progDesc "Compile and interpret an expression" ))

   <> command "interpret" (info (pure Interpret <**> helper)
        ( progDesc "Interpret an expression" ))

   <> command "samples" (info (Samples <$> esc <**> helper)
        ( progDesc "Pretty print samples" ))
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
