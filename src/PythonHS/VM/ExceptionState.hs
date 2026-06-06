module PythonHS.VM.ExceptionState (ExceptionState(..)) where

data ExceptionState = ExceptionState
  { exceptionHandlers :: [Int]
  , exceptionOutputs  :: [String]
  } deriving (Show)
