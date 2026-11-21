{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.CLI.CLIPart6Spec (spec) where

import Control.Exception (bracket)
import Data.List (isInfixOf, isPrefixOf)
import Paths_python_hs qualified
import PythonHS.CLI (replEvalLines, runFile)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist)
import System.Environment (getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runFile / replEvalLines - part 6" $ do
  it "exits interactive REPL when exit() is entered" $ do
    (code, out, _err) <- runInteractivePythonHs "print 1\nexit()\nprint 2\n"
    code `shouldBe` ExitSuccess
    ("1" `isInfixOf` out) `shouldBe` True
    ("2" `isInfixOf` out) `shouldBe` False

  it "prints expression result in interactive REPL executable" $ do
    (code, out, _err) <- runInteractivePythonHs "x = 10\nx + 5\nlen([1, 2]) + 3\n"
    code `shouldBe` ExitSuccess
    (">>> 15" `isInfixOf` out) `shouldBe` True
    (">>> 5" `isInfixOf` out) `shouldBe` True

  it "does not print None expression result in interactive REPL executable" $ do
    (code, out, _err) <- runInteractivePythonHs "None\n1\n"
    code `shouldBe` ExitSuccess
    (">>> >>> 1" `isInfixOf` out) `shouldBe` True

  it "prints string expression result with quotes in interactive REPL executable" $ do
    (code, out, _err) <- runInteractivePythonHs "\"hello\"\n"
    code `shouldBe` ExitSuccess
    (">>> 'hello'" `isInfixOf` out) `shouldBe` True

  it "escapes special characters in string expression result in interactive REPL executable" $ do
    (code, out, _err) <- runInteractivePythonHs "\"a'b\"\n\"a\tb\"\n"
    code `shouldBe` ExitSuccess
    (">>> 'a\\'b'" `isInfixOf` out) `shouldBe` True
    (">>> 'a\\tb'" `isInfixOf` out) `shouldBe` True

  it "prints dictionary string keys with quotes in interactive REPL executable" $ do
    (code, out, _err) <- runInteractivePythonHs "{\"k\": 1}\n"
    code `shouldBe` ExitSuccess
    (">>> {'k': 1}" `isInfixOf` out) `shouldBe` True

  it "starts interactive REPL with --engine vm" $ do
    (code, out, _err) <- runInteractivePythonHsWithArgs ["--engine", "vm"] "x = 1\nprint x\n"
    code `shouldBe` ExitSuccess
    ("1" `isInfixOf` out) `shouldBe` True
  where
    runInteractivePythonHs input = do
      exePath <- pythonHsExecutablePath
      readCreateProcessWithExitCode (proc exePath []) input

    runInteractivePythonHsWithArgs args input = do
      exePath <- pythonHsExecutablePath
      readCreateProcessWithExitCode (proc exePath args) input

    pythonHsExecutablePath = do
      binDir <- Paths_python_hs.getBinDir
      testExe <- getExecutablePath
      let baseDir = takeDirectory testExe
      let candidateDirs = take 12 (iterate takeDirectory baseDir)
      let candidatePaths =
            [binDir </> "python-hs"]
              ++ map (</> "x/python-hs/build/python-hs/python-hs") candidateDirs
              ++ map (</> "build/python-hs/python-hs") candidateDirs
      found <- findFirstExistingPath candidatePaths
      case found of
        Just path -> return path
        Nothing -> error "python-hs executable not found from Paths_python_hs.getBinDir or test binary layout"

    findFirstExistingPath [] = return Nothing
    findFirstExistingPath (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then return (Just candidate)
        else findFirstExistingPath rest

    restoreRunnerEngine previous =
      case previous of
        Nothing -> unsetEnv "PYTHON_HS_RUNNER_ENGINE"
        Just value -> setEnv "PYTHON_HS_RUNNER_ENGINE" value
