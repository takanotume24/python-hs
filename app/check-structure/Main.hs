module Main (main) where

import PythonHS.Structure.CheckStructureViolations (checkStructureViolations)
import System.Exit (exitFailure)

main :: IO ()
main = do
  violations <- checkStructureViolations "."
  if null violations
    then putStrLn "Structure check passed"
    else do
      putStrLn "Structure check failed:"
      mapM_ (putStrLn . ("- " ++)) violations
      exitFailure
