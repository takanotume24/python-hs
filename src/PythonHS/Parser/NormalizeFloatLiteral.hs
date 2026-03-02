module PythonHS.Parser.NormalizeFloatLiteral (normalizeFloatLiteral) where

normalizeFloatLiteral :: String -> String
normalizeFloatLiteral literal =
  let withLeading = if take 1 literal == "." then '0' : literal else literal
   in if not (null withLeading) && last withLeading == '.' then withLeading ++ "0" else withLeading
