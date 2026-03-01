module PythonHS.Runner.RunnerCaseCoverageHasMissing (runnerCaseCoverageHasMissing) where

runnerCaseCoverageHasMissing :: String -> Bool
runnerCaseCoverageHasMissing report =
  countAfter "=== COUNT PARITY ===" report > 0 || countAfter "=== COUNT VM ===" report > 0
  where
    countAfter :: String -> String -> Int
    countAfter marker text =
      case dropWhile (/= marker) (lines text) of
        _ : value : _ ->
          case reads value of
            [(n, "")] -> n
            _ -> 0
        _ -> 0