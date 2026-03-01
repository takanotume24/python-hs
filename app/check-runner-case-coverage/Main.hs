module Main (main) where

import PythonHS.Runner.RunnerCaseCoverageReport (runnerCaseCoverageReport)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [edgePath, parityPath, vmPath] -> do
      report <- runnerCaseCoverageReport edgePath parityPath vmPath
      putStr report
    _ -> do
      putStrLn "Usage: check-runner-case-coverage <RunnerEdgeSpec.hs> <RunnerVmParitySpec.hs> <RunSourceVmSpec.hs>"
      exitFailure
