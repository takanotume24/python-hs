module Test.CPython.LoadCaseIdsSpec (spec) where

import Data.List (sort)
import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.LoadCaseIds (loadCaseIds)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "loadCaseIds" $ do
  it "expands a file entry into every test method" $ do
    withSampleRepo $ \root -> do
      writeAllowlist root "test_sample.py"
      result <- loadCaseIds root (root </> "cases.allowlist")
      fmap sort result
        `shouldBe` Right
          ( sort
              [ caseId "FirstTest" "test_one",
                caseId "FirstTest" "test_two",
                caseId "SecondTest" "test_three"
              ]
          )

  it "expands a class entry into that class's test methods" $ do
    withSampleRepo $ \root -> do
      writeAllowlist root "test_sample.py::SecondTest"
      result <- loadCaseIds root (root </> "cases.allowlist")
      result `shouldBe` Right [caseId "SecondTest" "test_three"]

  it "keeps a method entry as a single case" $ do
    withSampleRepo $ \root -> do
      writeAllowlist root "test_sample.py::FirstTest::test_two"
      result <- loadCaseIds root (root </> "cases.allowlist")
      result `shouldBe` Right [caseId "FirstTest" "test_two"]

  it "reports an unknown class" $ do
    withSampleRepo $ \root -> do
      writeAllowlist root "test_sample.py::Missing"
      result <- loadCaseIds root (root </> "cases.allowlist")
      result `shouldBe` Left "Class not found or has no test methods: Missing"

  it "ignores comments and blank lines" $ do
    withSampleRepo $ \root -> do
      writeFile
        (root </> "cases.allowlist")
        "# comment\n\ntest_sample.py::FirstTest\n"
      result <- loadCaseIds root (root </> "cases.allowlist")
      result
        `shouldBe` Right
          [ caseId "FirstTest" "test_one",
            caseId "FirstTest" "test_two"
          ]
  where
    caseId className methodName =
      CaseId
        { caseIdFile = "test_sample.py",
          caseIdClassName = className,
          caseIdMethodName = methodName
        }

    withSampleRepo action =
      withSystemTempDirectory "load-case-ids" $ \root -> do
        writeFile
          (root </> "test_sample.py")
          ( unlines
              [ "class FirstTest:",
                "    def setUp(self):",
                "        pass",
                "    def test_one(self):",
                "        pass",
                "    def test_two(self):",
                "        pass",
                "class SecondTest:",
                "    def test_three(self):",
                "        pass"
              ]
          )
        action root

    writeAllowlist root entry =
      writeFile (root </> "cases.allowlist") (entry ++ "\n")
