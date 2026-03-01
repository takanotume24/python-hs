module Test.Runner.RunnerCaseCoverageReportSpec (spec) where

import qualified Paths_python_hs
import PythonHS.Runner.RunnerCaseCoverageReport (runnerCaseCoverageReport)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runnerCaseCoverageReport" $ do
  it "reports missing cases in parity and vm from RunnerEdge" $
    withSystemTempDirectory "runner-case-coverage" $ \root -> do
      let edgePath = root ++ "/test/Test/Runner/RunnerEdgeSpec.hs"
      let parityPath = root ++ "/test/Test/Runner/RunnerVmParitySpec.hs"
      let vmPath = root ++ "/test/Test/VM/RunSourceVmSpec.hs"
      createDirectoryIfMissing True (root ++ "/test/Test/Runner")
      createDirectoryIfMissing True (root ++ "/test/Test/VM")
      writeFile edgePath $ unlines
        [ "runSource \"A\\n\" `shouldBe` Right [\"A\"]"
        , "runSource \"B\\n\" `shouldBe` Right [\"B\"]"
        , "runSource \"C\\n\" `shouldBe` Right [\"C\"]"
        ]
      writeFile parityPath $ unlines
        [ "shouldMatchVm \"A\\n\""
        , "shouldMatchVm \"C\\n\""
        ]
      writeFile vmPath $ unlines
        [ "runSourceVm \"A\\n\" `shouldBe` Right [\"A\"]"
        ]

      report <- runnerCaseCoverageReport edgePath parityPath vmPath

      report
        `shouldBe` unlines
          [ "=== MISSING IN PARITY ==="
          , "B\\n"
          , "=== COUNT PARITY ==="
          , "1"
          , "=== MISSING IN VM ==="
          , "B\\n"
          , "C\\n"
          , "=== COUNT VM ==="
          , "2"
          ]

  it "check-runner-case-coverage exits failure when missing cases exist" $
    withSystemTempDirectory "runner-case-coverage-cli-fail" $ \root -> do
      let edgePath = root ++ "/test/Test/Runner/RunnerEdgeSpec.hs"
      let parityPath = root ++ "/test/Test/Runner/RunnerVmParitySpec.hs"
      let vmPath = root ++ "/test/Test/VM/RunSourceVmSpec.hs"
      createDirectoryIfMissing True (root ++ "/test/Test/Runner")
      createDirectoryIfMissing True (root ++ "/test/Test/VM")
      writeFile edgePath $ unlines
        [ "runSource \"A\\n\" `shouldBe` Right [\"A\"]"
        , "runSource \"B\\n\" `shouldBe` Right [\"B\"]"
        ]
      writeFile parityPath $ unlines
        [ "shouldMatchVm \"A\\n\""
        ]
      writeFile vmPath $ unlines
        [ "runSourceVm \"A\\n\" `shouldBe` Right [\"A\"]"
        ]

      exe <- checkRunnerCoverageExecutablePath
      (code, _out, _err) <- readCreateProcessWithExitCode (proc exe [edgePath, parityPath, vmPath]) ""
      code `shouldBe` ExitFailure 1

  it "check-runner-case-coverage exits success when no cases are missing" $
    withSystemTempDirectory "runner-case-coverage-cli-success" $ \root -> do
      let edgePath = root ++ "/test/Test/Runner/RunnerEdgeSpec.hs"
      let parityPath = root ++ "/test/Test/Runner/RunnerVmParitySpec.hs"
      let vmPath = root ++ "/test/Test/VM/RunSourceVmSpec.hs"
      createDirectoryIfMissing True (root ++ "/test/Test/Runner")
      createDirectoryIfMissing True (root ++ "/test/Test/VM")
      writeFile edgePath $ unlines
        [ "runSource \"A\\n\" `shouldBe` Right [\"A\"]"
        , "runSource \"B\\n\" `shouldBe` Right [\"B\"]"
        ]
      writeFile parityPath $ unlines
        [ "shouldMatchVm \"A\\n\""
        , "shouldMatchVm \"B\\n\""
        ]
      writeFile vmPath $ unlines
        [ "runSourceVm \"A\\n\" `shouldBe` Right [\"A\"]"
        , "runSourceVm \"B\\n\" `shouldBe` Right [\"B\"]"
        ]

      exe <- checkRunnerCoverageExecutablePath
      (code, _out, _err) <- readCreateProcessWithExitCode (proc exe [edgePath, parityPath, vmPath]) ""
      code `shouldBe` ExitSuccess
  where
    checkRunnerCoverageExecutablePath = do
      binDir <- Paths_python_hs.getBinDir
      testExe <- getExecutablePath
      let baseDir = takeDirectory testExe
      let candidateDirs = take 12 (iterate takeDirectory baseDir)
      let candidatePaths =
            [ binDir </> "check-runner-case-coverage"
            ]
              ++ map (</> "x/check-runner-case-coverage/build/check-runner-case-coverage/check-runner-case-coverage") candidateDirs
              ++ map (</> "build/check-runner-case-coverage/check-runner-case-coverage") candidateDirs
      found <- findFirstExistingPath candidatePaths
      case found of
        Just path -> return path
        Nothing -> error "check-runner-case-coverage executable not found"

    findFirstExistingPath [] = return Nothing
    findFirstExistingPath (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then return (Just candidate)
        else findFirstExistingPath rest
