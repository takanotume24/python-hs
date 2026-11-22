module Test.VM.RunSourceVmUnittestSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm unittest builtins" $ do
  it "accepts import unittest and a dotted TestCase base" $ do
    runSourceVm "import unittest\nclass T(unittest.TestCase):\n    pass\nprint(1)\n"
      `shouldBe` Right ["1"]

  it "accepts module, class, and method docstrings" $ do
    runSourceVm
      ( unlines
          [ "\"\"\"module docstring\"\"\"",
            "class T:",
            "    \"\"\"class docstring\"\"\"",
            "    def f(self):",
            "        \"\"\"method docstring\"\"\"",
            "        return 1",
            "print(T().f())"
          ]
      )
      `shouldBe` Right ["1"]

  it "runs assertion methods that pass" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_ok(self):",
            "        self.assertEqual(1 + 1, 2)",
            "        self.assertEqual(1, 1.0)",
            "        self.assertNotEqual(1, 2)",
            "        self.assertTrue(True)",
            "        self.assertFalse(False)",
            "        self.assertIsNone(None)",
            "        self.assertIsNotNone(1)",
            "        self.assertIn(2, [1, 2, 3])",
            "        self.assertNotIn(4, [1, 2, 3])",
            "        self.assertIs(1, 1)",
            "        self.assertIsNot(1, 2)",
            "T().test_ok()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["done"]

  it "reports assertEqual failures" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_fail(self):",
            "        self.assertEqual(1, 2)",
            "T().test_fail()"
          ]
      )
      `shouldBe` Left "AssertionError: 1 != 2 at 4:14"

  it "reports assertTrue failures" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_fail(self):",
            "        self.assertTrue(False)",
            "T().test_fail()"
          ]
      )
      `shouldBe` Left "AssertionError: expected True at 4:14"

  it "treats setUp and tearDown as no-ops when not defined" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_ok(self):",
            "        self.setUp()",
            "        self.tearDown()",
            "T().test_ok()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["done"]

  it "prefers a user-defined setUp over the builtin no-op" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def setUp(self):",
            "        print(\"setup\")",
            "    def test_ok(self):",
            "        self.setUp()",
            "T().test_ok()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["setup", "done"]
