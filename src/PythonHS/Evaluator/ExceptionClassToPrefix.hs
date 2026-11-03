module PythonHS.Evaluator.ExceptionClassToPrefix (exceptionClassToPrefix) where

-- | Maps a builtin exception class name to its runtime error prefix,
-- including a trailing space so the message can be appended directly.
exceptionClassToPrefix :: String -> String
exceptionClassToPrefix className
  | className == "RuntimeError" = "Runtime error: "
  | className == "TypeError" = "Type error: "
  | className == "ValueError" = "Value error: "
  | className == "NameError" = "Name error: "
  | className == "ArgumentError" = "Argument error: "
  | className == "ImportError" = "Import error: "
  | className == "UnboundLocalError" = "UnboundLocalError: "
  | className == "AttributeError" = "Attribute error: "
  | className == "AssertionError" = "AssertionError: "
  | otherwise = "Runtime error: "
