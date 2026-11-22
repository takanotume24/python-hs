module PythonHS.VM.SplitByDot (splitByDot) where

splitByDot :: String -> [String]
splitByDot text =
  case break (== '.') text of
    (segment, []) -> [segment]
    (segment, '.' : rest) -> segment : splitByDot rest
    _ -> [text]
