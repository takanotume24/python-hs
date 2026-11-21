module PythonHS.VM.ExceptionApplyResult (ExceptionApplyResult (..)) where

import PythonHS.Evaluator.Value (Value)

data ExceptionApplyResult = ExceptionApplyResult
  { exceptionApplyResultError :: Maybe String,
    exceptionApplyResultIp :: Int,
    exceptionApplyResultStack :: [Value],
    exceptionApplyResultHandlers :: [Int],
    exceptionApplyResultLocalChanges :: [(String, Maybe Value)]
  }
  deriving (Eq, Show)
