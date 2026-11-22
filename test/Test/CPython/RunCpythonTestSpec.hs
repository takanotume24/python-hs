module Test.CPython.RunCpythonTestSpec (spec) where

import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.RunCpythonTest (runCpythonTest)
import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runCpythonTest" $ do
  it "runs an allowlisted test method and reports success" $ do
    withSystemTempDirectory "python-hs-cpython-test" $ \repoRoot -> do
      let testFile = repoRoot </> "third_party/cpython/Lib/test/test_sample.py"
          allowlistFile = repoRoot </> "allowlist"
      createDirectoryIfMissing True (repoRoot </> "third_party/cpython/Lib/test")
      writeFile
        testFile
        ( unlines
            [ "import unittest",
              "",
              "class T(unittest.TestCase):",
              "    def test_ok(self):",
              "        self.assertEqual(1, 1)"
            ]
        )
      writeFile allowlistFile "third_party/cpython/Lib/test/test_sample.py::T::test_ok\n"
      result <-
        runCpythonTest
          RunCpythonTestConfig
            { runCpythonTestConfigRepoRoot = repoRoot,
              runCpythonTestConfigAllowlistPath = allowlistFile
            }
      result
        `shouldBe` Right
          [ CaseReport
              { caseReportId =
                  CaseId
                    { caseIdFile = "third_party/cpython/Lib/test/test_sample.py",
                      caseIdClassName = "T",
                      caseIdMethodName = "test_ok"
                    },
                caseReportFailure = Nothing
              }
          ]
