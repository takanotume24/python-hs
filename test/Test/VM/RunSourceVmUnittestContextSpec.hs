module Test.VM.RunSourceVmUnittestContextSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm unittest context helpers" $ do
  it "runs addCleanup after tearDown" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_ok(self):",
            "        def cleanup():",
            "            print(\"cleanup\")",
            "        self.addCleanup(cleanup)",
            "        print(\"body\")",
            "test_case = T()",
            "test_case.test_ok()",
            "test_case.tearDown()",
            "test_case.__python_hs_run_cleanups__()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["body", "cleanup", "done"]

  it "supports aliases of assertion methods" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_alias(self):",
            "        eq = self.assertEqual",
            "        eq(1 + 1, 2)",
            "T().test_alias()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["done"]

  it "supports assertRaisesRegex as a context manager" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_raises(self):",
            "        with self.assertRaisesRegex(ValueError, \"boom\"):",
            "            raise ValueError(\"boom\")",
            "T().test_raises()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["done"]

  it "supports subTest as a context manager" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_sub(self):",
            "        with self.subTest(value=1):",
            "            self.assertEqual(1, 1)",
            "T().test_sub()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["done"]

  it "supports assertIsInstance and assertGreater" $ do
    runSourceVm
      ( unlines
          [ "import unittest",
            "class T(unittest.TestCase):",
            "    def test_more(self):",
            "        self.assertIsInstance(\"x\", str)",
            "        self.assertNotIsInstance(1, str)",
            "        self.assertGreater(2, 1)",
            "        self.assertGreaterEqual(2, 2)",
            "        self.assertLess(1, 2)",
            "        self.assertLessEqual(2, 2)",
            "T().test_more()",
            "print(\"done\")"
          ]
      )
      `shouldBe` Right ["done"]
