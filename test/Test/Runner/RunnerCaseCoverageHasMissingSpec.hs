module Test.Runner.RunnerCaseCoverageHasMissingSpec (spec) where

import PythonHS.Runner.RunnerCaseCoverageHasMissing (runnerCaseCoverageHasMissing)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runnerCaseCoverageHasMissing" $ do
  it "returns False when both parity and vm missing counts are zero" $ do
    let report =
          unlines
            [ "=== MISSING IN PARITY ==="
            , "=== COUNT PARITY ==="
            , "0"
            , "=== MISSING IN VM ==="
            , "=== COUNT VM ==="
            , "0"
            ]
    runnerCaseCoverageHasMissing report `shouldBe` False

  it "returns True when parity missing count is non-zero" $ do
    let report =
          unlines
            [ "=== MISSING IN PARITY ==="
            , "case-a"
            , "=== COUNT PARITY ==="
            , "1"
            , "=== MISSING IN VM ==="
            , "=== COUNT VM ==="
            , "0"
            ]
    runnerCaseCoverageHasMissing report `shouldBe` True

  it "returns True when vm missing count is non-zero" $ do
    let report =
          unlines
            [ "=== MISSING IN PARITY ==="
            , "=== COUNT PARITY ==="
            , "0"
            , "=== MISSING IN VM ==="
            , "case-b"
            , "=== COUNT VM ==="
            , "2"
            ]
    runnerCaseCoverageHasMissing report `shouldBe` True