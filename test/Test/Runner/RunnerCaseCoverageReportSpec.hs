module Test.Runner.RunnerCaseCoverageReportSpec (spec) where

import PythonHS.Runner.RunnerCaseCoverageReport (runnerCaseCoverageReport)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
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
