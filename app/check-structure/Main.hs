module Main (main) where

import PythonHS.Structure.CheckStructure (checkStructureViolations, checkStructureWarnings)
import System.Exit (exitFailure)

main :: IO ()
main = do
  warnings <- checkStructureWarnings "."
  if null warnings
    then pure ()
    else do
      putStrLn "Structure check warnings:"
      mapM_ (putStrLn . ("- " ++)) warnings
  violations <- checkStructureViolations "."
  if null violations
    then putStrLn "Structure check passed"
    else do
      putStrLn "Structure check failed:"
      mapM_ (putStrLn . ("- " ++)) violations
      exitFailure
