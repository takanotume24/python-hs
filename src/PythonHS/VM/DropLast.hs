module PythonHS.VM.DropLast (dropLast) where

dropLast :: [a] -> [a]
dropLast segments =
  case segments of
    [] -> []
    [_] -> []
    x : xs -> x : dropLast xs
