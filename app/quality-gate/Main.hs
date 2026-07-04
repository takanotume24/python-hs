module Main (main) where

import Data.List (intercalate)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  let gateDefs =
        [ ("cabal test", ["cabal", "test"]),
          ("check-structure", ["cabal", "run", "check-structure"]),
          ("ormolu", ["ormolu_check_placeholder"]),
          ("hlint", ["hlint", "src", "app"]),
          ("detect-positional-args", ["cabal", "run", "detect-positional-args", "--", "src"])
        ]
  results <- mapM goGate gateDefs
  let failed = filter (not . snd) results
  putStrLn ""
  putStrLn "=== Quality Gate Summary ==="
  mapM_ (\(name, passed) -> putStrLn $ "  [" ++ (if passed then "PASS" else "FAIL") ++ "] " ++ name) results
  putStrLn ""
  if null failed
    then putStrLn "All quality gates passed!" >> exitWith ExitSuccess
    else putStrLn ("Failed: " ++ intercalate ", " (map fst failed)) >> exitWith (ExitFailure 1)
  where
    goGate (name, cmdArgs) = do
      putStrLn $ ">>> " ++ name
      hFlush stdout
      (code, out, err) <- goCmd cmdArgs
      let passed = code == ExitSuccess
      if passed
        then return (name, True)
        else do
          let output = if null err then out else err
          putStrLn $ "  FAILED: " ++ take 500 (if null output then "(no output)" else output)
          return (name, False)

    goCmd ("ormolu_check_placeholder" : _) = do
      (_, out, _) <- readProcessWithExitCode "git" ["ls-files", "*.hs"] ""
      let files = filter (not . null) (lines out)
      if null files
        then return (ExitSuccess, "", "")
        else readProcessWithExitCode "ormolu" ("--mode" : "check" : files) ""
    goCmd (cmd : args) = readProcessWithExitCode cmd args ""
    goCmd [] = return (ExitFailure 1, "", "no command")
