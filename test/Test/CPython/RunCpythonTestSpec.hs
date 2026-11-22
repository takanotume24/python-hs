module Test.CPython.RunCpythonTestSpec (spec) where

import PythonHS.CPython.CaseFailure (CaseFailure (..))
import PythonHS.CPython.CaseFailureCategory (CaseFailureCategory (AssertionFailure))
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
              runCpythonTestConfigAllowlistPath = allowlistFile,
              runCpythonTestConfigFileMode = False
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

  it "classifies a failing assertion" $ do
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
              "    def test_fail(self):",
              "        self.assertEqual(1, 2)"
            ]
        )
      writeFile allowlistFile "third_party/cpython/Lib/test/test_sample.py::T::test_fail\n"
      result <-
        runCpythonTest
          RunCpythonTestConfig
            { runCpythonTestConfigRepoRoot = repoRoot,
              runCpythonTestConfigAllowlistPath = allowlistFile,
              runCpythonTestConfigFileMode = False
            }
      case result of
        Left err -> fail err
        Right [CaseReport {caseReportFailure = Just CaseFailure {caseFailureCategory = category}}] ->
          category `shouldBe` AssertionFailure
        Right other -> fail ("unexpected result: " ++ show other)

  it "calls setUp automatically before an allowlisted method" $ do
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
              "    def setUp(self):",
              "        global prepared",
              "        prepared = 41",
              "    def test_ok(self):",
              "        self.assertEqual(prepared + 1, 42)"
            ]
        )
      writeFile allowlistFile "third_party/cpython/Lib/test/test_sample.py::T::test_ok\n"
      result <-
        runCpythonTest
          RunCpythonTestConfig
            { runCpythonTestConfigRepoRoot = repoRoot,
              runCpythonTestConfigAllowlistPath = allowlistFile,
              runCpythonTestConfigFileMode = False
            }
      case result of
        Left err -> fail err
        Right [CaseReport {caseReportFailure = Nothing}] -> pure ()
        Right other -> fail ("unexpected result: " ++ show other)
