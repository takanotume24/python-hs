module PythonHS.VM.DecoratorApplyResult (DecoratorApplyResult (..)) where

import PythonHS.VM.Instruction (Instruction)

data DecoratorApplyResult = DecoratorApplyResult
  { decoratorApplyResultCode :: [Instruction],
    decoratorApplyResultNextIndex :: Int
  }
  deriving (Eq, Show)
