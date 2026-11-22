module PythonHS.CPython.RunCpythonTestOptions (RunCpythonTestOptions (..)) where

import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig)

-- | Parsed command-line options for @run-cpython-test@.
data RunCpythonTestOptions = RunCpythonTestOptions
  { runCpythonTestOptionsConfig :: RunCpythonTestConfig,
    runCpythonTestOptionsJson :: Bool,
    runCpythonTestOptionsList :: Bool
  }
