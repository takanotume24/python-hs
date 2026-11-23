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
              "__python_hs_case.setUp()",
              "__python_hs_case.test_ok()",
              "__python_hs_case.tearDown()",
              "__python_hs_case.__python_hs_run_cleanups__()",
              "print(\"__PYTHON_HS_CPYTHON_TEST_OK__\")"
            ]
        )

  it "reports a missing class" $ do
    buildDriverSource "import unittest\n" CaseId {caseIdFile = "test.py", caseIdClassName = "T", caseIdMethodName = "test_ok"}
      `shouldBe` Left "Class not found: T"

  it "does not hoist imports that are indented inside methods" $ do
    let source =
          unlines
            [ "import unittest",
              "",
              "class T(unittest.TestCase):",
              "    def test_ok(self):",
              "        def inner():",
              "            import sys",
              "        self.assertEqual(1, 1)"
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
              "        def inner():",
              "            import sys",
              "        self.assertEqual(1, 1)",
              "__python_hs_case = T()",
              "__python_hs_case.setUp()",
              "__python_hs_case.test_ok()",
              "__python_hs_case.tearDown()",
              "__python_hs_case.__python_hs_run_cleanups__()",
              "print(\"__PYTHON_HS_CPYTHON_TEST_OK__\")"
            ]
        )

  it "takes setUp from the target class only" $ do
    let source =
          unlines
            [ "import unittest",
              "",
              "class Helper:",
              "    def setUp(self):",
              "        print(\"helper\")",
              "    def test_ok(self):",
              "        self.fail()",
              "",
              "class T(unittest.TestCase):",
              "    def setUp(self):",
              "        print(\"target\")",
              "    def test_ok(self):",
              "        self.assertEqual(1, 1)"
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
              "    def setUp(self):",
              "        print(\"target\")",
              "    def test_ok(self):",
              "        self.assertEqual(1, 1)",
              "__python_hs_case = T()",
              "__python_hs_case.setUp()",
              "__python_hs_case.test_ok()",
              "__python_hs_case.tearDown()",
              "__python_hs_case.__python_hs_run_cleanups__()",
              "print(\"__PYTHON_HS_CPYTHON_TEST_OK__\")"
            ]
        )
