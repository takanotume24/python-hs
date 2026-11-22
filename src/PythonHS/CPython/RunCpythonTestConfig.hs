module PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..)) where

-- | Configuration for the @run-cpython-test@ executable.
data RunCpythonTestConfig = RunCpythonTestConfig
  { runCpythonTestConfigRepoRoot :: FilePath,
    runCpythonTestConfigAllowlistPath :: FilePath,
    runCpythonTestConfigFileMode :: Bool
  }
