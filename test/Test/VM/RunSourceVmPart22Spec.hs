module Test.VM.RunSourceVmPart22Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 22" $ do
  describe "pow builtin" $ do
    it "returns an integer power for non-negative exponents" $ do
      runSourceVm "print pow(2, 10)\n" `shouldBe` Right ["1024"]

    it "returns a float for a negative exponent" $ do
      runSourceVm "print pow(2, -1)\n" `shouldBe` Right ["0.5"]

    it "returns a modular power when the third argument is given" $ do
      runSourceVm "print pow(2, 10, 1000)\n" `shouldBe` Right ["24"]

    it "matches the ** operator for floats" $ do
      runSourceVm "print pow(2.0, 3)\n" `shouldBe` Right ["8.0"]

    it "reports a value error for a zero modulus" $ do
      runSourceVm "print pow(2, 3, 0)\n" `shouldBe` Left "Value error: pow modulus must not be zero at 1:7"

    it "reports a value error for a negative exponent with modulus" $ do
      runSourceVm "print pow(2, -1, 5)\n" `shouldBe` Left "Value error: pow with modulus requires a non-negative exponent at 1:7"

    it "reports type error for a non-number base" $ do
      runSourceVm "print pow('a', 2)\n" `shouldBe` Left "Type error: pow expects number arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print pow(2)\n" `shouldBe` Left "Argument count mismatch when calling pow at 1:7"

  describe "divmod builtin" $ do
    it "returns quotient and remainder for positive integers" $ do
      runSourceVm "print divmod(7, 3)\n" `shouldBe` Right ["(2, 1)"]

    it "floors toward negative infinity" $ do
      runSourceVm "print divmod(-7, 3)\n" `shouldBe` Right ["(-3, 2)"]

    it "keeps the remainder sign aligned with the divisor" $ do
      runSourceVm "print divmod(7, -3)\n" `shouldBe` Right ["(-3, -2)"]

    it "returns floats when either argument is a float" $ do
      runSourceVm "print divmod(7.5, 2)\n" `shouldBe` Right ["(3.0, 1.5)"]

    it "reports a value error on integer division by zero" $ do
      runSourceVm "print divmod(1, 0)\n" `shouldBe` Left "Value error: division by zero at 1:7"

    it "reports type error for a non-number argument" $ do
      runSourceVm "print divmod('a', 2)\n" `shouldBe` Left "Type error: divmod expects number arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print divmod(1)\n" `shouldBe` Left "Argument count mismatch when calling divmod at 1:7"

  describe "reversed builtin" $ do
    it "reverses a list" $ do
      runSourceVm "print reversed([1, 2, 3])\n" `shouldBe` Right ["[3, 2, 1]"]

    it "reverses a tuple into a list" $ do
      runSourceVm "print reversed((1, 2, 3))\n" `shouldBe` Right ["[3, 2, 1]"]

    it "reverses the characters of a string" $ do
      runSourceVm "print reversed('abc')\n" `shouldBe` Right ["[c, b, a]"]

    it "returns an empty list for an empty iterable" $ do
      runSourceVm "print reversed([])\n" `shouldBe` Right ["[]"]

    it "reports type error for a non-iterable" $ do
      runSourceVm "print reversed(5)\n" `shouldBe` Left "Type error: reversed expects iterable at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print reversed()\n" `shouldBe` Left "Argument count mismatch when calling reversed at 1:7"
