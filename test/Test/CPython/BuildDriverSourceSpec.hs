module Test.CPython.BuildDriverSourceSpec (spec) where

import PythonHS.CPython.BuildDriverSource (buildDriverSource)
import PythonHS.CPython.CaseId (CaseId (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "buildDriverSource" $ do
  it "copies imports, the class header, and only the requested method" $ do
    let source =
          unlines
            [ "import unittest",
              "",
              "class T(unittest.TestCase):",
              "    def test_ok(self):",
              "        self.assertEqual(1, 1)",
              "",
              "    def test_other(self):",
              "        self.fail()"
            ]
        caseId =
          CaseId
            { caseIdFile = "test.py",
              caseIdClassName = "T",
              caseIdMethodName = "test_ok"
            }
    buildDriverSource source caseId
      `shouldBe` Right
        ( unlines
            [ "import unittest",
              "class T(unittest.TestCase):",
              "    def test_ok(self):",
              "        self.assertEqual(1, 1)",
              "",
              "__python_hs_case = T()",
              "__python_hs_case.test_ok()",
              "print(\"__PYTHON_HS_CPYTHON_TEST_OK__\")"
            ]
        )

  it "reports a missing class" $ do
    buildDriverSource "import unittest\n" CaseId {caseIdFile = "test.py", caseIdClassName = "T", caseIdMethodName = "test_ok"}
      `shouldBe` Left "Class not found: T"
