module PythonHS.VM.ModuleKeyFor (moduleKeyFor) where

moduleKeyFor :: [String] -> String
moduleKeyFor segments = joinWith "." segments
  where
    joinWith _ [] = ""
    joinWith _ [single] = single
    joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs
