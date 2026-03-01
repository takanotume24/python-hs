module PythonHS.CLI.RunFile (runFile) where

import Control.Exception (IOException, try)
import PythonHS.Runner (runSourceWithEngine)
import PythonHS.Runner.ResolveRunnerEngine (resolveRunnerEngine)
import System.Environment (lookupEnv)

runFile :: FilePath -> IO (Either String [String])
runFile path = do
  envEngine <- lookupEnv "PYTHON_HS_RUNNER_ENGINE"
  eres <- try (readFile path) :: IO (Either IOException String)
  case eres of
    Left e -> return $ Left (show e)
    Right contents -> return $ runSourceWithEngine (resolveRunnerEngine envEngine) contents