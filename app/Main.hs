module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import PythonHS.CLI (runFile, startRepl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> startRepl
    [path] -> do
      result <- runFile path
      case result of
        Left err -> putStrLn err >> exitFailure
        Right outs -> mapM_ putStrLn outs
    _ -> putStrLn "Usage: python-hs [file]" >> exitFailure
