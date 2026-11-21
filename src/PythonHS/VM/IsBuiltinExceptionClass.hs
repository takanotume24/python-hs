module PythonHS.VM.IsBuiltinExceptionClass (isBuiltinExceptionClass) where

isBuiltinExceptionClass :: String -> Bool
isBuiltinExceptionClass className =
  className
    `elem` [ "BaseException",
             "Exception",
             "RuntimeError",
             "TypeError",
             "ValueError",
             "NameError",
             "ArgumentError",
             "ImportError",
             "UnboundLocalError",
             "AttributeError",
             "AssertionError"
           ]
