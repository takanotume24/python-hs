module Test.VM.RunSourceVmUnaryPrecedenceSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm unary precedence" $ do
  it "binds exponentiation tighter than unary minus" $ do
    runSourceVm "print(-2 ** 3)\nprint(-2 ** 4)\nprint((-2) ** 4)\n"
      `shouldBe` Right ["-8", "-16", "16"]

  it "binds exponentiation tighter than bitwise not" $ do
    runSourceVm "print(~2 ** 1)\n"
      `shouldBe` Right ["-3"]

  it "keeps unary minus on the exponent working" $ do
    runSourceVm "print(2 ** -1)\n"
      `shouldBe` Right ["0.5"]
