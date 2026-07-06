module PythonHS.AST.FunctionDefault (FunctionDefault (..)) where

data FunctionDefault e = FunctionDefault
  { functionDefaultParam :: String,
    functionDefaultExpr :: e
  }
  deriving (Eq, Show)
