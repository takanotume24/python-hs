module PythonHS.Lexer.ScanOperatorTokenConfig (ScanOperatorTokenConfig (..)) where

import PythonHS.Lexer.Position (Position)

-- | A single character and the remaining input for operator scanning.
data ScanOperatorTokenConfig = ScanOperatorTokenConfig
  { scanOperatorTokenCharacter :: Char,
    scanOperatorTokenRest :: String,
    scanOperatorTokenPosition :: Position
  }
