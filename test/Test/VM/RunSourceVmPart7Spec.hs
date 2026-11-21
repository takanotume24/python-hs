module Test.VM.RunSourceVmPart7Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 7" $ do
  describe "abs builtin" $ do
    it "returns absolute value of int" $ do
      runSourceVm "print abs(-3)\nprint abs(3)\nprint abs(0)\n" `shouldBe` Right ["3", "3", "0"]

    it "returns absolute value of float" $ do
      runSourceVm "print abs(-2.5)\nprint abs(2.5)\n" `shouldBe` Right ["2.5", "2.5"]

    it "reports type error for non-number" $ do
      runSourceVm "print abs('x')\n" `shouldBe` Left "Type error: abs expects int or float at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print abs()\n" `shouldBe` Left "Argument count mismatch when calling abs at 1:7"

  describe "min builtin" $ do
    it "returns minimum of positional arguments" $ do
      runSourceVm "print min(3, 1, 2)\n" `shouldBe` Right ["1"]

    it "returns minimum of a list" $ do
      runSourceVm "print min([3, 1, 2])\n" `shouldBe` Right ["1"]

    it "returns minimum of a tuple" $ do
      runSourceVm "print min((3, 1, 2))\n" `shouldBe` Right ["1"]

    it "compares int and float as numbers" $ do
      runSourceVm "print min(1.5, 2)\n" `shouldBe` Right ["1.5"]

    it "returns minimum of strings" $ do
      runSourceVm "print min('b', 'a')\n" `shouldBe` Right ["a"]

    it "reports value error for empty list" $ do
      runSourceVm "print min([])\n" `shouldBe` Left "Value error: min() arg is an empty sequence at 1:7"

    it "reports type error for mixed incomparable values" $ do
      runSourceVm "print min(1, 'a')\n" `shouldBe` Left "Type error: min expects int, float, or string at 1:7"

    it "reports type error for a single non-iterable argument" $ do
      runSourceVm "print min(5)\n" `shouldBe` Left "Type error: min expects list or tuple at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print min()\n" `shouldBe` Left "Argument count mismatch when calling min at 1:7"

  describe "max builtin" $ do
    it "returns maximum of positional arguments" $ do
      runSourceVm "print max(3, 1, 2)\n" `shouldBe` Right ["3"]

    it "returns maximum of a list" $ do
      runSourceVm "print max([3, 1, 2])\n" `shouldBe` Right ["3"]

    it "returns maximum of strings" $ do
      runSourceVm "print max('a', 'c', 'b')\n" `shouldBe` Right ["c"]

    it "reports value error for empty list" $ do
      runSourceVm "print max([])\n" `shouldBe` Left "Value error: max() arg is an empty sequence at 1:7"

  describe "sum builtin" $ do
    it "sums list of ints" $ do
      runSourceVm "print sum([1, 2, 3])\n" `shouldBe` Right ["6"]

    it "sums empty list to zero" $ do
      runSourceVm "print sum([])\n" `shouldBe` Right ["0"]

    it "sums mixed int and float" $ do
      runSourceVm "print sum([1.5, 2])\n" `shouldBe` Right ["3.5"]

    it "sums with a start value" $ do
      runSourceVm "print sum([1, 2], 10)\n" `shouldBe` Right ["13"]

    it "reports type error for non-number element" $ do
      runSourceVm "print sum([1, 'a'])\n" `shouldBe` Left "Type error: sum expects list of number at 1:7"

    it "reports type error for non-list argument" $ do
      runSourceVm "print sum(5)\n" `shouldBe` Left "Type error: sum expects list or tuple at 1:7"

    it "reports type error for non-number start value" $ do
      runSourceVm "print sum([1], 'x')\n" `shouldBe` Left "Type error: sum expects number as second argument at 1:7"

  describe "sorted builtin" $ do
    it "sorts a list ascending" $ do
      runSourceVm "print sorted([3, 1, 2])\n" `shouldBe` Right ["[1, 2, 3]"]

    it "sorts a tuple into a list" $ do
      runSourceVm "print sorted((3, 1, 2))\n" `shouldBe` Right ["[1, 2, 3]"]

    it "sorts empty list" $ do
      runSourceVm "print sorted([])\n" `shouldBe` Right ["[]"]

    it "sorts mixed int and float as numbers" $ do
      runSourceVm "print sorted([2, 1.5])\n" `shouldBe` Right ["[1.5, 2]"]

    it "does not mutate the original list" $ do
      runSourceVm "xs = [3, 1, 2]\nprint sorted(xs)\nprint xs\n" `shouldBe` Right ["[1, 2, 3]", "[3, 1, 2]"]

    it "reports type error for non-list argument" $ do
      runSourceVm "print sorted('abc')\n" `shouldBe` Left "Type error: sorted expects list or tuple at 1:7"

    it "reports type error for non-number element" $ do
      runSourceVm "print sorted([1, 'a'])\n" `shouldBe` Left "Type error: sorted expects list of number at 1:7"
