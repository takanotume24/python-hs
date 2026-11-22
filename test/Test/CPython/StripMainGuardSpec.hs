module Test.CPython.StripMainGuardSpec (spec) where

import PythonHS.CPython.StripMainGuard (stripMainGuard)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "stripMainGuard" $ do
  it "removes a trailing main guard block" $ do
    stripMainGuard
      ( unlines
          [ "import unittest",
            "",
            "if __name__ == \"__main__\":",
            "    unittest.main()"
          ]
      )
      `shouldBe` unlines ["import unittest", ""]

  it "keeps code before and after an unrelated if statement" $ do
    stripMainGuard
      ( unlines
          [ "x = 1",
            "if x:",
            "    print(x)"
          ]
      )
      `shouldBe` unlines ["x = 1", "if x:", "    print(x)"]
