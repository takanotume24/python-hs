{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.CLI.CLIPart1Spec (spec) where

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
spec = describe "runFile / replEvalLines - part 1" $ do
  it "runs a file and returns printed output" $
    withSystemTempFile "script.pyhs" $ \path h -> do
      hPutStr h "x = 1\nprint x\n"
      hClose h
      res <- runFile path
      res `shouldBe` Right ["1"]

  it "runs an MVP scenario from file" $
    withSystemTempFile "mvp.pyhs" $ \path h -> do
      hPutStr h "x = 1\nif x:\nprint 10\nelse:\nprint 20\nwhile x < 3:\nx = x + 1\ndef id(v):\nreturn v\nprint id(x)\n"
      hClose h
      res <- runFile path
      res `shouldBe` Right ["10", "3"]

  it "uses vm engine for runFile when PYTHON_HS_RUNNER_ENGINE=vm" $
    withSystemTempFile "vm-engine.pyhs" $ \path h -> do
      hPutStr h "def add(a, b = 2):\n  return a + b\nprint add(1)\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["3"]

  it "runs import math script in vm engine for runFile" $
    withSystemTempFile "vm-math.pyhs" $ \path h -> do
      hPutStr h "import math\nprint math.sqrt(9)\nprint math.pi()\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["3.0", "3.141592653589793"]

  it "runs remaining math functions in vm engine for runFile" $
    withSystemTempFile "vm-math-more.pyhs" $ \path h -> do
      hPutStr h "import math\nprint math.sin(0)\nprint math.cos(0)\nprint math.tan(0)\nprint math.log(1)\nprint math.exp(1)\nprint math.e()\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["0.0", "1.0", "0.0", "0.0", "2.718281828459045", "2.718281828459045"]

  it "runs import math alias script in vm engine for runFile" $
    withSystemTempFile "vm-math-alias.pyhs" $ \path h -> do
      hPutStr h "import math as m\nprint m.sqrt(9)\nprint m.pi()\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["3.0", "3.141592653589793"]

  it "runs from math import alias script in vm engine for runFile" $
    withSystemTempFile "vm-math-from.pyhs" $ \path h -> do
      hPutStr h "from math import sqrt as s, e\nprint s(9)\nprint e\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["3.0", "2.718281828459045"]

  it "runs local .py module import in vm engine for runFile" $
    withSystemTempDirectory "vm-local-import" $ \dir -> do
      let modulePath = dir </> "helper.py"
      let mainPath = dir </> "main.py"
      writeFile modulePath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import helper\nprint helper.inc(4)\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["5"]

  it "keeps LEGB-style local precedence over global in vm engine for runFile" $
    withSystemTempFile "vm-legb-local-global.pyhs" $ \path h -> do
      hPutStr h "x = 1\ndef f():\n  x = 2\n  return x\nprint f()\nprint x\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["2", "1"]

  it "keeps class method binding parity for obj.f and A.f(obj, ...) in vm engine for runFile" $
    withSystemTempFile "vm-method-binding-parity.pyhs" $ \path h -> do
      hPutStr h "class A:\n  def add(self, x):\n    return x + 1\na = A()\nprint a.add(2)\nprint A.add(a, 3)\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["3", "4"]

  it "evaluates call arguments left-to-right in vm engine for runFile" $
    withSystemTempFile "vm-eval-order-args.pyhs" $ \path h -> do
      hPutStr h "def a():\n  print 1\n  return 10\ndef b():\n  print 2\n  return 20\ndef f(x, y):\n  return x + y\nprint f(a(), b())\n"
      hClose h
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile path
        res `shouldBe` Right ["1", "2", "30"]

  it "runs module top-level once across repeated imports in vm engine for runFile" $
    withSystemTempDirectory "vm-import-reimport-once" $ \dir -> do
      let modulePath = dir </> "helper.py"
      let mainPath = dir </> "main.py"
      writeFile modulePath "print 7\n"
      writeFile mainPath "import helper\nimport helper\nprint 1\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["7", "1"]

  it "runs local .py from-import with alias in vm engine for runFile" $
    withSystemTempDirectory "vm-local-from-import" $ \dir -> do
      let modulePath = dir </> "helper.py"
      let mainPath = dir </> "main.py"
      writeFile modulePath "base = 10\ndef add(x):\n  return x + base\n"
      writeFile mainPath "from helper import add as a, base\nprint a(2)\nprint base\n"
      bracket (lookupEnv "PYTHON_HS_RUNNER_ENGINE") restoreRunnerEngine $ \_ -> do
        setEnv "PYTHON_HS_RUNNER_ENGINE" "vm"
        res <- runFile mainPath
        res `shouldBe` Right ["12", "10"]

  it "runs package __init__.py import in vm engine for runFile" $
    withSystemTempDirectory "vm-local-package-import" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let mainPath = dir </> "main.py"
      createDirectory packageDir
      writeFile initPath "def inc(x):\n  return x + 1\n"
      writeFile mainPath "import pkg\nprint pkg.inc(4)\n"
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
