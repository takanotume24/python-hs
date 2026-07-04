module PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..)) where

-- | Configuration for running VM source code with search paths.
data RunSourceVmWithSearchPathsConfig = RunSourceVmWithSearchPathsConfig
  { runSourceVmWithSearchPathsSearchPaths :: [FilePath],
    runSourceVmWithSearchPathsSource :: String
  }
