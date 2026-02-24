module PythonHS.Evaluator.ShowPos (showPos) where

import PythonHS.Lexer.Position (Position (Position))

-- format position as "line:column"
showPos :: Position -> String
showPos (Position l c) = show l ++ ":" ++ show c
