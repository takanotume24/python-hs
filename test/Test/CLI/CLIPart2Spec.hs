{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.CLI.CLIPart2Spec (spec) where

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
spec = describe "runFile / replEvalLines - part 2" $ do
  it "runs from package import in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-package-import" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "base = 10\ndef add(x):\n  return x + base\n"
      writeFile mainPath "from pkg import add as a, base\nprint a(2)\nprint base\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["12", "10"]

  it "does not bind bare submodule name for import pkg.sub in vm engine for runFile" $
    withSystemTempDirectory "vm-local-submodule-import" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg.sub\nprint sub.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Left "Name error: undefined identifier sub at 2:7"

  it "runs package submodule import via pkg.sub receiver in vm engine for runFile" $
    withSystemTempDirectory "vm-local-submodule-import-pkg-receiver" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg.sub\nprint pkg.sub.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "binds package and submodule values for import pkg.sub in vm engine for runFile" $
    withSystemTempDirectory "vm-local-submodule-import-module-values" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg.sub\nprint pkg\nprint pkg.sub\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["<module:pkg>", "<module:pkg.sub>"]

  it "supports module-object member call after alias assignment for import pkg.sub as s in vm engine for runFile" $
    withSystemTempDirectory "vm-local-submodule-import-alias-assignment" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg.sub as s\nm = s\nprint m.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "supports module-object chain call via reassigned package value in vm engine for runFile" $
    withSystemTempDirectory "vm-local-package-object-reassignment" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg.sub\nx = pkg\nprint x.sub.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "supports deep module alias call for import pkg.sub.deep as d in vm engine for runFile" $
    withSystemTempDirectory "vm-local-deep-module-alias-call" $ \dir -> do
      let packageDir = dir </> "pkg"
      let subDir = packageDir </> "sub"
      let initPath = packageDir </> "__init__.py"
      let subInitPath = subDir </> "__init__.py"
      let deepPath = subDir </> "deep.py"
      let mainPath = dir </> "main.py"
      createDirectoryIfMissing True subDir
      writeFile initPath "pass\n"
      writeFile subInitPath "pass\n"
      writeFile deepPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg.sub.deep as d\nprint d.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "runs from package import submodule in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-package-submodule-import" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "from pkg import sub\nprint sub.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "prefers package attribute over submodule on from-import in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-import-attribute-priority" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "name.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "name = 41\n"
      writeFile subPath "def value():\n  return 99\n"
      writeFile mainPath "from pkg import name\nprint name\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["41"]
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
