module PythonHS.Runner.RunnerCaseCoverageReportConfig (RunnerCaseCoverageReportConfig (..)) where

-- | Configuration for generating runner case coverage report.
data RunnerCaseCoverageReportConfig = RunnerCaseCoverageReportConfig
  { runnerCaseCoverageReportEdgePath :: FilePath,
    runnerCaseCoverageReportParityPath :: FilePath,
    runnerCaseCoverageReportVmPath :: FilePath
  }
