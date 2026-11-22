module Test.CPython.RunCpythonTestFileModeSpec (spec) where

import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.RunCpythonTest (runCpythonTest)
import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runCpythonTest file mode" $ do
  it "reports the same outcome for extraction and whole-file modes" $ do
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
              "        self.assertEqual(1 + 1, 2)",
              "",
              "if __name__ == \"__main__\":",
              "    unittest.main()"
            ]
        )
      writeFile allowlistFile "third_party/cpython/Lib/test/test_sample.py::T::test_ok\n"
      let runWith fileMode =
            runCpythonTest
              RunCpythonTestConfig
                { runCpythonTestConfigRepoRoot = repoRoot,
                  runCpythonTestConfigAllowlistPath = allowlistFile,
                  runCpythonTestConfigFileMode = fileMode
                }
      extracted <- runWith False
      wholeFile <- runWith True
      wholeFile `shouldBe` extracted

  it "runs whole-file mode with module-level helpers" $ do
    withSystemTempDirectory "python-hs-cpython-test" $ \repoRoot -> do
      let testFile = repoRoot </> "third_party/cpython/Lib/test/test_sample.py"
          allowlistFile = repoRoot </> "allowlist"
      createDirectoryIfMissing True (repoRoot </> "third_party/cpython/Lib/test")
      writeFile
        testFile
        ( unlines
            [ "import unittest",
              "",
              "def double(value):",
              "    return value * 2",
              "",
              "class T(unittest.TestCase):",
              "    def test_ok(self):",
              "        self.assertEqual(double(21), 42)",
              "",
              "if __name__ == \"__main__\":",
              "    unittest.main()"
            ]
        )
      writeFile allowlistFile "third_party/cpython/Lib/test/test_sample.py::T::test_ok\n"
      result <-
        runCpythonTest
          RunCpythonTestConfig
            { runCpythonTestConfigRepoRoot = repoRoot,
              runCpythonTestConfigAllowlistPath = allowlistFile,
              runCpythonTestConfigFileMode = True
            }
      case result of
        Left err -> fail err
        Right [CaseReport {caseReportFailure = Nothing}] -> pure ()
        Right other -> fail ("unexpected result: " ++ show other)

  it "prefers the cpython-shim module over the CPython library" $ do
    withSystemTempDirectory "python-hs-cpython-test" $ \repoRoot -> do
      let testFile = repoRoot </> "third_party/cpython/Lib/test/test_sample.py"
          allowlistFile = repoRoot </> "allowlist"
      createDirectoryIfMissing True (repoRoot </> "third_party/cpython/Lib/test")
      createDirectoryIfMissing True (repoRoot </> "cpython-shim")
      writeFile (repoRoot </> "cpython-shim/keyword.py") "def iskeyword(word):\n    return True\n"
      writeFile (repoRoot </> "third_party/cpython/Lib/keyword.py") "def iskeyword(word):\n    return False\n"
      writeFile
        testFile
        ( unlines
            [ "import unittest",
              "import keyword",
              "",
              "class T(unittest.TestCase):",
              "    def test_shim(self):",
              "        self.assertTrue(keyword.iskeyword('anything'))"
            ]
        )
      writeFile allowlistFile "third_party/cpython/Lib/test/test_sample.py::T::test_shim\n"
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
