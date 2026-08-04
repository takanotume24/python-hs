module PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalStmtRet = EvalStmtRet
  { evalStmtRetValue :: Value,
    evalStmtRetPos :: Position
  }
  deriving (Eq, Show)
