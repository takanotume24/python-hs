module PythonHS.VM.ModulePrefixFor (modulePrefixFor) where

modulePrefixFor :: [String] -> String
modulePrefixFor segments = "__python_hs_local_" ++ joinWith "_" segments ++ "__"
  where
    joinWith _ [] = ""
    joinWith _ [single] = single
    joinWith sep (x : xs) = x ++ sep ++ joinWith sep xs
