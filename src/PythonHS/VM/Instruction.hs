module PythonHS.VM.Instruction (Instruction (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data Instruction
  = PushConst Value
  | LoadName String Position
  | StoreName String
  | AddValues Position
  | JumpIfFalse Int
  | Jump Int
  | PrintTop
  | Halt
  deriving (Eq, Show)
