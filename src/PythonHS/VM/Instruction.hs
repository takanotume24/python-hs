module PythonHS.VM.Instruction (Instruction (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data Instruction
  = PushConst Value
  | LoadName String Position
  | StoreName String
  | ApplyUnaryMinus Position
  | ApplyNot Position
  | ApplyBinary BinaryOperator Position
  | JumpIfFalse Int
  | Jump Int
  | DefineFunction String [String] [Instruction]
  | CallFunction String Int Position
  | ReturnTop
  | PrintTop
  | Halt
  deriving (Eq, Show)
