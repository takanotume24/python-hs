module PythonHS.VM.LoopContext (LoopContext (..)) where

data LoopContext = LoopContext
  { loopContextBreakTarget :: Int,
    loopContextContinueTarget :: Int
  }
  deriving (Eq, Show)
