module PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)

-- | Result of a single lexing step.
data ScanTokenStepResult = ScanTokenStepResult
  { scanTokenStepResultToken :: Token,
    scanTokenStepResultRemaining :: String,
    scanTokenStepResultNextPosition :: Position
  }
