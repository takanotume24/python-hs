{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.CLI.CLIPart4Spec (spec) where

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
spec = describe "runFile / replEvalLines - part 4" $ do
  it "runs builtin module import for os marker in vm engine for runFile" $
    withSystemTempFile "vm-import-os.pyhs" $ \path h -> do
      hPutStr h "import os\nprint os\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["<module:os>"]

  it "runs minimal json module functions in vm engine for runFile" $
    withSystemTempFile "vm-import-json-functions.pyhs" $ \path h -> do
      hPutStr h "import json\nprint json.dumps(1)\nprint json.loads(\"1\")\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["1", "1"]

  it "runs minimal pathlib Path constructor in vm engine for runFile" $
    withSystemTempFile "vm-import-pathlib-functions.pyhs" $ \path h -> do
      hPutStr h "import pathlib\nprint pathlib.Path(\"a/b\")\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["a/b"]

  it "runs minimal os getcwd function in vm engine for runFile" $
    withSystemTempFile "vm-import-os-functions.pyhs" $ \path h -> do
      hPutStr h "import os\nprint os.getcwd()\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["."]

  it "reports name-not-found error for from-import missing member in vm engine for runFile" $
    withSystemTempDirectory "vm-from-import-missing-member" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "x = 1\n"
      writeFile mainPath "from pkg import missing\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Left "Import error: name not found missing in pkg"

  it "reports name-not-found error for nested from-import missing member in vm engine for runFile" $
    withSystemTempDirectory "vm-from-import-missing-nested-member" $ \dir -> do
      let packageDir = dir </> "pkg"
      let subDir = packageDir </> "sub"
      let initPath = packageDir </> "__init__.py"
      let subInitPath = subDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectoryIfMissing True subDir
      writeFile initPath "pass\n"
      writeFile subInitPath "x = 1\n"
      writeFile mainPath "from pkg.sub import missing\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Left "Import error: name not found missing in pkg.sub"

  it "supports two-node circular import with partially initialized modules in vm engine for runFile" $
    withSystemTempDirectory "vm-circular-import-two-node" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let aPath = packageDir </> "a.py"
      let bPath = packageDir </> "b.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile aPath "import pkg.b\ndef a():\n  return 1\n"
      writeFile bPath "import pkg.a\ndef b():\n  return 2\n"
      writeFile mainPath "from pkg.a import a\nprint a()\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["1"]

  it "supports three-node circular import with partially initialized modules in vm engine for runFile" $
    withSystemTempDirectory "vm-circular-import-three-node" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let aPath = packageDir </> "a.py"
      let bPath = packageDir </> "b.py"
      let cPath = packageDir </> "c.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile aPath "import pkg.b\ndef a():\n  return 1\n"
      writeFile bPath "import pkg.c\ndef b():\n  return 2\n"
      writeFile cPath "import pkg.a\ndef c():\n  return 3\n"
      writeFile mainPath "from pkg.a import a\nprint a()\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["1"]

  it "handles try/except and raise in vm engine for runFile" $
    withSystemTempFile "vm-try-except-raise.pyhs" $ \path h -> do
      hPutStr h "try:\n  raise \"boom\"\nexcept:\n  print 1\nprint 2\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["1", "2"]

  it "handles try/except for non-raise runtime error in vm engine for runFile" $
    withSystemTempFile "vm-try-except-runtime.pyhs" $ \path h -> do
      hPutStr h "try:\n  print 1 / 0\nexcept:\n  print 7\nprint 8\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["7", "8"]

  it "handles try/except/finally in vm engine for runFile" $
    withSystemTempFile "vm-try-except-finally.pyhs" $ \path h -> do
      hPutStr h "try:\n  print 1\nexcept:\n  print 9\nfinally:\n  print 2\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["1", "2"]

  it "handles multiple except suites in vm engine for runFile" $
    withSystemTempFile "vm-try-multi-except.pyhs" $ \path h -> do
      hPutStr h "try:\n  raise \"boom\"\nexcept:\n  print 1\nexcept:\n  print 2\nfinally:\n  print 3\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["1", "3"]
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
