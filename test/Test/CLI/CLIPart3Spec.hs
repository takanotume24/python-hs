{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.CLI.CLIPart3Spec (spec) where

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
spec = describe "runFile / replEvalLines - part 3" $ do
  it "runs from package submodule import member in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-submodule-member-import" $ \dir -> do
      let packageDir = dir </> "pkg"
      let nestedDir = packageDir </> "sub"
      let initPath = packageDir </> "__init__.py"
      let nestedInitPath = nestedDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      createDirectoryIfMissing True nestedDir
      writeFile initPath "pass\n"
      writeFile nestedInitPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "from pkg.sub import inc as i\nprint i(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "runs deep package submodule import via pkg.sub.deep receiver in vm engine for runFile" $
    withSystemTempDirectory "vm-local-deep-submodule-import" $ \dir -> do
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
      writeFile mainPath "import pkg.sub.deep\nprint pkg.sub.deep.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "runs from package submodule import deep module in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-submodule-import-deep" $ \dir -> do
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
      writeFile mainPath "from pkg.sub import deep\nprint deep.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "runs from deep package submodule import member in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-deep-submodule-member-import" $ \dir -> do
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
      writeFile mainPath "from pkg.sub.deep import inc as i\nprint i(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "reports math type error in vm engine for runFile" $
    withSystemTempFile "vm-math-type-error.pyhs" $ \path h -> do
      hPutStr h "import math\nprint math.sqrt(\"x\")\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldSatisfy` (== Left "Type error: sqrt expects number at 2:12")

  it "reports module-not-found import error in vm engine for runFile" $
    withSystemTempFile "vm-import-unsupported.pyhs" $ \path h -> do
      hPutStr h "import ssl\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Left "Import error: module not found ssl"

  it "reports unsupported relative import in vm engine for runFile" $
    withSystemTempFile "vm-relative-import-unsupported.pyhs" $ \path h -> do
      hPutStr h "from . import x\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Left "Import error: relative import with no known parent package"

  it "runs star import from package in vm engine for runFile" $
    withSystemTempDirectory "vm-star-import-package" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "x = 1\n_y = 2\n"
      writeFile mainPath "from pkg import *\nprint x\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["1"]

  it "respects __all__ for star import from package in vm engine for runFile" $
    withSystemTempDirectory "vm-star-import-package-all" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "def visible():\n  return 1\ndef hidden():\n  return 2\n__all__ = [\"visible\"]\n"
      writeFile mainPath "from pkg import *\nprint visible()\nprint hidden()\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldSatisfy` (== Left "Name error: undefined function hidden at 3:7")

  it "runs relative import inside package module via pkg.mod receiver in vm engine for runFile" $
    withSystemTempDirectory "vm-relative-import-in-package" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let utilPath = packageDir </> "util.py"
      let modPath = packageDir </> "mod.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile utilPath "def inc(x):\n  return x + 1\n"
      writeFile modPath "from . import util\ndef run(x):\n  return util.inc(x)\n"
      writeFile mainPath "import pkg.mod\nprint pkg.mod.run(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]
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
