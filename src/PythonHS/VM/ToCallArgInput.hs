module PythonHS.VM.ToCallArgInput (ToCallArgInput (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)

data ToCallArgInput = ToCallArgInput
  { toCallArgInputCode :: [Instruction],
    toCallArgInputKeyword :: Maybe String,
    toCallArgInputPos :: Position
  }
  deriving (Eq, Show)
