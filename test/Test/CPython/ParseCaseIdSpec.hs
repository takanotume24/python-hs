module Test.CPython.ParseCaseIdSpec (spec) where

import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.ParseCaseId (parseCaseId)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parseCaseId" $ do
  it "parses a file, class, and method" $ do
    parseCaseId "third_party/cpython/Lib/test/test_unary.py::UnaryOpTestCase::test_invert"
      `shouldBe` Right
        CaseId
          { caseIdFile = "third_party/cpython/Lib/test/test_unary.py",
            caseIdClassName = "UnaryOpTestCase",
            caseIdMethodName = "test_invert"
          }

  it "trims surrounding whitespace" $ do
    parseCaseId "  test.py::T::test_ok  "
      `shouldBe` Right
        CaseId
          { caseIdFile = "test.py",
            caseIdClassName = "T",
            caseIdMethodName = "test_ok"
          }

  it "rejects entries without a method" $ do
    parseCaseId "test.py::T"
      `shouldBe` Left "Invalid test case identifier: test.py::T"
