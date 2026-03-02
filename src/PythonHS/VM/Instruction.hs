module PythonHS.VM.Instruction (Instruction (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data Instruction
  = PushConst Value
  | LoadName String Position
  | StoreName String
  | BuildList Int
  | BuildDict Int
  | DeclareGlobal String
  | LoopGuard Position
  | ForSetup Int Position
  | ForNext String Int Position
  | PushExceptionHandler Int
  | PushFinallyHandler Int
  | PopExceptionHandler
  | ApplyUnaryMinus Position
  | ApplyNot Position
  | ApplyBinary BinaryOperator Position
  | MatchPattern Pattern Position
  | JumpIfFalse Int
  | Jump Int
  | DefineFunction String [String] [(String, [Instruction])] [Instruction]
  | CallFunction String [([Instruction], Maybe String, Position)] Position
  | RaiseTop Position
  | RaisePendingError
  | ReturnTop
  | PrintTop
  | Halt
  deriving (Eq, Show)
