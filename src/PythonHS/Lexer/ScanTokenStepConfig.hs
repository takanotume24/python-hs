module PythonHS.Lexer.ScanTokenStepConfig (ScanTokenStepConfig (..)) where

import PythonHS.Lexer.Position (Position)

-- | Configuration for a single lexing step.
data ScanTokenStepConfig = ScanTokenStepConfig
  { scanTokenStepSource :: String,
    scanTokenStepPosition :: Position
  }
