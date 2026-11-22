module Test.VM.RunSourceVmBlankLineSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm blank lines" $ do
  it "ignores blank lines between top-level statements" $ do
    runSourceVm "x = 1\n\ny = 2\nprint(x + y)\n" `shouldBe` Right ["3"]

  it "ignores blank lines inside a function body" $ do
    runSourceVm "def add(a, b):\n    total = a + b\n\n    return total\n\nprint(add(1, 2))\n" `shouldBe` Right ["3"]

  it "ignores blank lines between a header and its indented body" $ do
    runSourceVm "if True:\n\n    print(1)\n" `shouldBe` Right ["1"]

  it "ignores blank lines before a dedent" $ do
    runSourceVm "if True:\n    print(1)\n\nprint(2)\n" `shouldBe` Right ["1", "2"]

  it "keeps line numbers stable after blank lines" $ do
    runSourceVm "x = 1\n\n\nprint(missing)\n" `shouldBe` Left "Name error: undefined identifier missing at 4:7"
