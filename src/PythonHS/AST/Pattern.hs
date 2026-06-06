module PythonHS.AST.Pattern (Pattern (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

data Pattern
  = ValuePattern
      { valuePatternExpr :: Expr,
        valuePatternPos :: Position
      }
  | WildcardPattern { wildcardPatternPos :: Position }
  | CapturePattern
      { capturePatternName :: String,
        capturePatternPos :: Position
      }
  | AsPattern
      { asPatternInner :: Pattern,
        asPatternAlias :: String,
        asPatternPos :: Position
      }
  | OrPattern
      { orPatternItems :: [Pattern],
        orPatternPos :: Position
      }
  | SequencePattern
      { sequencePatternItems :: [Pattern],
        sequencePatternRest :: Maybe String,
        sequencePatternPos :: Position
      }
  | MappingPattern
      { mappingPatternPairs :: [(Expr, Pattern)],
        mappingPatternRest :: Maybe String,
        mappingPatternPos :: Position
      }
  deriving (Eq, Show)
