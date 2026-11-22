module Test.CPython.ExtractTestCasesSpec (spec) where

import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.ExtractTestCases (extractTestCases)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "extractTestCases" $ do
  it "extracts test methods from every class" $ do
    extractTestCases
      "test_sample.py"
      ( unlines
          [ "class FirstTest:",
            "    def test_one(self):",
            "        pass",
            "    def helper(self):",
            "        pass",
            "class SecondTest:",
            "    def test_two(self):",
            "        pass"
          ]
      )
      `shouldBe` [ caseId "FirstTest" "test_one",
                   caseId "SecondTest" "test_two"
                 ]

  it "ignores setUp and tearDown" $ do
    extractTestCases
      "test_sample.py"
      ( unlines
          [ "class T:",
            "    def setUp(self):",
            "        pass",
            "    def test_ok(self):",
            "        pass",
            "    def tearDown(self):",
            "        pass"
          ]
      )
      `shouldBe` [caseId "T" "test_ok"]

  it "returns no cases when the source has no test classes" $ do
    extractTestCases "test_sample.py" "x = 1\n"
      `shouldBe` []

  it "does not pick up nested definitions as class tests" $ do
    extractTestCases
      "test_sample.py"
      ( unlines
          [ "class T:",
            "    def test_outer(self):",
            "        def test_inner():",
            "            pass"
          ]
      )
      `shouldBe` [caseId "T" "test_outer"]
  where
    caseId className methodName =
      CaseId
        { caseIdFile = "test_sample.py",
          caseIdClassName = className,
          caseIdMethodName = methodName
        }
