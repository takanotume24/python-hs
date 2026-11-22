module PythonHS.VM.MatchesExceptionClass (matchesExceptionClass) where

import Data.List (isPrefixOf)

-- | Return whether an error message corresponds to @expected@.
--
-- The VM reports exceptions as @\"<Prefix>: message at line:column\"@ strings.
-- Builtin exception names are normalized to the same prefixes used by
-- 'raiseTopError'; user-defined classes fall back to @\"ClassName: \"@.
matchesExceptionClass :: String -> String -> Bool
matchesExceptionClass expected err =
  expected == "Exception"
    || expected == "BaseException"
    || prefix `isPrefixOf` err
    || (expected == "TypeError" && argumentErrorPrefix `isPrefixOf` err)
  where
    argumentErrorPrefix = "Argument "
    prefix = case expected of
      "RuntimeError" -> "Runtime error:"
      "TypeError" -> "Type error:"
      "ValueError" -> "Value error:"
      "NameError" -> "Name error:"
      "ArgumentError" -> "Argument error:"
      "ImportError" -> "Import error:"
      "UnboundLocalError" -> "UnboundLocalError:"
      "AttributeError" -> "Attribute error:"
      "AssertionError" -> "AssertionError:"
      "ZeroDivisionError" -> "Zero division error:"
      "IndexError" -> "Index error:"
      "KeyError" -> "Key error:"
      "SyntaxError" -> "Syntax error:"
      other -> other ++ ": "
