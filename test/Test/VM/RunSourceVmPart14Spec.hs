module Test.VM.RunSourceVmPart14Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 14" $ do
  describe "round builtin" $ do
    it "rounds a float down when below the midpoint" $ do
      runSourceVm "print round(1.4)\n" `shouldBe` Right ["1"]

    it "rounds a float up when above the midpoint" $ do
      runSourceVm "print round(1.5)\n" `shouldBe` Right ["2"]

    it "rounds ties to the nearest even integer" $ do
      runSourceVm "print round(2.5)\n" `shouldBe` Right ["2"]

    it "rounds negative ties to the nearest even integer" $ do
      runSourceVm "print round(-1.5)\n" `shouldBe` Right ["-2"]

    it "keeps an int unchanged without ndigits" $ do
      runSourceVm "print round(7)\n" `shouldBe` Right ["7"]

    it "rounds an int to the nearest ten" $ do
      runSourceVm "print round(12, -1)\n" `shouldBe` Right ["10"]

    it "rounds an int tie up to an even ten" $ do
      runSourceVm "print round(15, -1)\n" `shouldBe` Right ["20"]

    it "rounds an int tie down to an even ten" $ do
      runSourceVm "print round(25, -1)\n" `shouldBe` Right ["20"]

    it "rounds a float to two decimal places" $ do
      runSourceVm "print round(1.234, 2)\n" `shouldBe` Right ["1.23"]

    it "returns a float when ndigits is zero" $ do
      runSourceVm "print round(1.5, 0)\nprint round(2.5, 0)\n" `shouldBe` Right ["2.0", "2.0"]

    it "reports type error for a non-number" $ do
      runSourceVm "print round('a')\n" `shouldBe` Left "Type error: round expects int or float at 1:7"

    it "reports type error for a non-int ndigits" $ do
      runSourceVm "print round(1, 'a')\n" `shouldBe` Left "Type error: round expects int as second argument at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print round()\n" `shouldBe` Left "Argument count mismatch when calling round at 1:7"

  describe "chr builtin" $ do
    it "returns the character for an uppercase code point" $ do
      runSourceVm "print chr(65)\n" `shouldBe` Right ["A"]

    it "returns the character for a lowercase code point" $ do
      runSourceVm "print chr(97)\n" `shouldBe` Right ["a"]

    it "returns the character for a digit code point" $ do
      runSourceVm "print chr(48)\n" `shouldBe` Right ["0"]

    it "reports type error for a non-int" $ do
      runSourceVm "print chr('A')\n" `shouldBe` Left "Type error: chr expects int at 1:7"

    it "reports value error for a negative code point" $ do
      runSourceVm "print chr(-1)\n" `shouldBe` Left "Value error: chr() arg not in range at 1:7"

    it "reports value error for a code point above the Unicode range" $ do
      runSourceVm "print chr(1114112)\n" `shouldBe` Left "Value error: chr() arg not in range at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print chr()\n" `shouldBe` Left "Argument count mismatch when calling chr at 1:7"

  describe "ord builtin" $ do
    it "returns the code point of an uppercase character" $ do
      runSourceVm "print ord('A')\n" `shouldBe` Right ["65"]

    it "returns the code point of a lowercase character" $ do
      runSourceVm "print ord('a')\n" `shouldBe` Right ["97"]

    it "returns the code point of a digit character" $ do
      runSourceVm "print ord('0')\n" `shouldBe` Right ["48"]

    it "reports type error for an empty string" $ do
      runSourceVm "print ord('')\n" `shouldBe` Left "Type error: ord() expected a character at 1:7"

    it "reports type error for a string of length two" $ do
      runSourceVm "print ord('ab')\n" `shouldBe` Left "Type error: ord() expected a character at 1:7"

    it "reports type error for a non-string" $ do
      runSourceVm "print ord(5)\n" `shouldBe` Left "Type error: ord() expected a character at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print ord()\n" `shouldBe` Left "Argument count mismatch when calling ord at 1:7"
