module Test.CLI.CLIPart7Spec (spec) where

import Data.List (isInfixOf)
import Paths_python_hs qualified
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "input builtin - part 7" $ do
  it "reads a line from standard input" $
    runScript "print input()\n" "hello\n" $ \code out _ -> do
      code `shouldBe` ExitSuccess
      out `shouldBe` "hello\n"

  it "reads consecutive lines" $
    runScript "a = input()\nb = input()\nprint a + b\n" "foo\nbar\n" $ \code out _ -> do
      code `shouldBe` ExitSuccess
      out `shouldBe` "foobar\n"

  it "accepts a prompt argument" $
    runScript "print input('name: ')\n" "bob\n" $ \code out _ -> do
      code `shouldBe` ExitSuccess
      out `shouldBe` "bob\n"

  it "reports EOF as a runtime error" $
    runScript "print input()\n" "" $ \code out _ -> do
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` isInfixOf "Runtime error: EOF when reading a line at 1:7"

  it "reports type error for a non-string prompt" $
    runScript "print input(5)\n" "x\n" $ \code out _ -> do
      code `shouldBe` ExitFailure 1
      out `shouldSatisfy` isInfixOf "Type error: input expects string prompt at 1:7"
  where
    runScript source stdinText assertion =
      withSystemTempFile "input.pyhs" $ \path handle -> do
        hPutStr handle source
        hClose handle
        exePath <- pythonHsExecutablePath
        (code, out, err) <- readCreateProcessWithExitCode (proc exePath ["--engine", "vm", path]) stdinText
        assertion code out err

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
      if exists then return (Just candidate) else findFirstExistingPath rest
