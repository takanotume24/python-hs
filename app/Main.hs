module Main (main) where

import System.Environment (getArgs)
import System.Environment (setEnv)
import System.Exit (exitFailure)
import PythonHS.CLI (runFile, startRepl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--engine", rawEngine] -> do
      setRunnerEngine rawEngine
      startRepl
    ["--engine", rawEngine, path] -> do
      setRunnerEngine rawEngine
      runFileAndPrint path
    [] -> startRepl
    [path] -> runFileAndPrint path
    _ -> putStrLn "Usage: python-hs [--engine ast|vm] [file]" >> exitFailure
  where
    runFileAndPrint path = do
      result <- runFile path
      case result of
        Left err -> putStrLn err >> exitFailure
        Right outs -> mapM_ putStrLn outs

    setRunnerEngine rawEngine =
      case rawEngine of
        "vm" -> setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        "ast" -> setEnv "PYTHON_HS_RUNNER_ENGINE" "ast"
        _ -> setEnv "PYTHON_HS_RUNNER_ENGINE" "ast"
