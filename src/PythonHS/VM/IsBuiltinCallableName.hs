module PythonHS.VM.IsBuiltinCallableName (isBuiltinCallableName) where

import PythonHS.Lexer.Position (Position (..))
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))

-- | Return whether @name@ refers to a builtin function.
--
-- The check calls the builtin dispatcher with no arguments. Unknown names
-- yield 'Nothing'; known names yield 'Just' even when the empty argument list
-- is invalid, which is enough to treat the name as a callable value.
--
-- A few names (such as @math@ constants) are dispatched with a module
-- receiver and must not be treated as bare callables.
isBuiltinCallableName :: String -> Bool
isBuiltinCallableName name =
  name `notElem` moduleReceiverOnlyNames
    && case callBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = [], callBuiltinPos = placeholderPosition} of
      Just _ -> True
      Nothing -> False
  where
    placeholderPosition = Position {line = 0, column = 0}
    moduleReceiverOnlyNames = ["pi", "e"]
