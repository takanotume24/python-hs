module PythonHS.VM.MethodContextBindings (methodContextBindings) where

import PythonHS.Evaluator.Value (Value (StringValue))

-- | Bindings injected into a method body so that zero-argument @super()@ can
-- find the class that owns the running method and the receiver it belongs to.
methodContextBindings :: String -> Value -> [(String, Value)]
methodContextBindings className selfValue =
  [ ("__python_hs_current_class__", StringValue className),
    ("__python_hs_current_instance__", selfValue)
  ]
