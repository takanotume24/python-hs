module PythonHS.CLI.RunFile (runFile) where

import Control.Exception (IOException, try)
import PythonHS.RunSource (runSource)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.Runner.ResolveRunnerEngine (resolveRunnerEngine)
import PythonHS.Runner.RunnerEngine (RunnerEngine (AstEngine, VmEngine))
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)

runFile :: FilePath -> IO (Either String [String])
runFile path = do
  envEngine <- lookupEnv "PYTHON_HS_RUNNER_ENGINE"
  eres <- try (readFile path) :: IO (Either IOException String)
  case eres of
    Left e -> return $ Left (show e)
    Right contents ->
      case resolveRunnerEngine envEngine of
        AstEngine -> pure (runSource contents)
        VmEngine -> runSourceVmWithSearchPaths [takeDirectory path] contents