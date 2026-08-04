module PythonHS.VM.CompiledCallArg (CompiledCallArg (..)) where

import PythonHS.Lexer.Position (Position)

data CompiledCallArg instruction = CompiledCallArg
  { compiledCallArgCode :: [instruction],
    compiledCallArgKeyword :: Maybe String,
    compiledCallArgPos :: Position
  }
  deriving (Eq, Show)
