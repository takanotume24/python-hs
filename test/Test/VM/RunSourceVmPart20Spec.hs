module Test.VM.RunSourceVmPart20Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 20" $ do
  describe "startswith with start/end" $ do
    it "checks only the slice from start" $ do
      runSourceVm "print 'hello'.startswith('ll', 2)\n" `shouldBe` Right ["1"]

    it "returns 0 when the prefix is outside the start" $ do
      runSourceVm "print 'hello'.startswith('he', 1)\n" `shouldBe` Right ["0"]

    it "checks only the slice up to end" $ do
      runSourceVm "print 'hello'.startswith('hell', 0, 4)\n" `shouldBe` Right ["1"]

    it "returns 0 when the prefix extends past end" $ do
      runSourceVm "print 'hello'.startswith('hello', 0, 4)\n" `shouldBe` Right ["0"]

    it "adjusts a negative start by the string length" $ do
      runSourceVm "print 'hello'.startswith('lo', -2)\n" `shouldBe` Right ["1"]

    it "adjusts a negative end by the string length" $ do
      runSourceVm "print 'hello'.startswith('ell', 1, -1)\n" `shouldBe` Right ["1"]

    it "clamps an end past the string length" $ do
      runSourceVm "print 'hello'.startswith('lo', 3, 99)\n" `shouldBe` Right ["1"]

    it "returns 0 when start is past the string length" $ do
      runSourceVm "print 'hello'.startswith('', 6)\n" `shouldBe` Right ["0"]

    it "reports type error for a non-int start" $ do
      runSourceVm "print startswith('hello', 'h', 'x')\n" `shouldBe` Left "Type error: startswith expects int start/end at 1:7"

  describe "endswith with start/end" $ do
    it "checks only the slice up to end" $ do
      runSourceVm "print 'hello'.endswith('ell', 0, 4)\n" `shouldBe` Right ["1"]

    it "returns 0 when the suffix is past end" $ do
      runSourceVm "print 'hello'.endswith('lo', 0, 4)\n" `shouldBe` Right ["0"]

    it "adjusts negative start/end" $ do
      runSourceVm "print 'hello'.endswith('ll', -4, -1)\n" `shouldBe` Right ["1"]

    it "returns 0 when start is past the string length" $ do
      runSourceVm "print 'hello'.endswith('', 6)\n" `shouldBe` Right ["0"]

    it "reports type error for a non-int end" $ do
      runSourceVm "print endswith('hello', 'o', 0, 'x')\n" `shouldBe` Left "Type error: endswith expects int start/end at 1:7"

  describe "find with start/end" $ do
    it "searches from start" $ do
      runSourceVm "print 'hello'.find('l', 3)\n" `shouldBe` Right ["3"]

    it "returns -1 when the match is before start" $ do
      runSourceVm "print 'hello'.find('he', 1)\n" `shouldBe` Right ["-1"]

    it "does not find a match that extends past end" $ do
      runSourceVm "print 'hello'.find('lo', 0, 4)\n" `shouldBe` Right ["-1"]

    it "adjusts a negative start" $ do
      runSourceVm "print 'hello'.find('l', -2)\n" `shouldBe` Right ["3"]

    it "returns start for an empty needle within the range" $ do
      runSourceVm "print 'hello'.find('', 2)\n" `shouldBe` Right ["2"]

    it "returns -1 for an empty needle past the string length" $ do
      runSourceVm "print 'hello'.find('', 6)\n" `shouldBe` Right ["-1"]

    it "reports type error for a non-int start" $ do
      runSourceVm "print find('hello', 'l', 'x')\n" `shouldBe` Left "Type error: find expects int start/end at 1:7"

  describe "rfind with start/end" $ do
    it "returns the last index of the substring" $ do
      runSourceVm "print 'hello'.rfind('l')\n" `shouldBe` Right ["3"]

    it "returns -1 when the substring is absent" $ do
      runSourceVm "print 'hello'.rfind('z')\n" `shouldBe` Right ["-1"]

    it "searches only within the end" $ do
      runSourceVm "print 'hello'.rfind('l', 0, 3)\n" `shouldBe` Right ["2"]

    it "returns end for an empty needle" $ do
      runSourceVm "print 'hello'.rfind('', 0, 2)\n" `shouldBe` Right ["2"]

  describe "index and rindex" $ do
    it "returns the first index of the substring" $ do
      runSourceVm "print index('hello', 'l')\n" `shouldBe` Right ["2"]

    it "returns the last index for rindex" $ do
      runSourceVm "print rindex('hello', 'l')\n" `shouldBe` Right ["3"]

    it "reports value error when index does not find the substring" $ do
      runSourceVm "print index('hello', 'z')\n" `shouldBe` Left "Value error: substring not found at 1:7"

    it "reports value error when rindex does not find the substring" $ do
      runSourceVm "print rindex('hello', 'z')\n" `shouldBe` Left "Value error: substring not found at 1:7"

    it "supports start/end for index" $ do
      runSourceVm "print index('hello', 'l', 3)\n" `shouldBe` Right ["3"]

    it "reports value error when start skips the only match" $ do
      runSourceVm "print index('hello', 'he', 1)\n" `shouldBe` Left "Value error: substring not found at 1:7"

  describe "count with start/end" $ do
    it "counts only within the end" $ do
      runSourceVm "print 'hello'.count('l', 0, 4)\n" `shouldBe` Right ["2"]

    it "counts only after start" $ do
      runSourceVm "print 'hello'.count('l', 3)\n" `shouldBe` Right ["1"]

    it "adjusts a negative start" $ do
      runSourceVm "print 'hello'.count('l', -2)\n" `shouldBe` Right ["1"]

    it "counts an empty needle as the slice length plus one" $ do
      runSourceVm "print 'hello'.count('', 1, 3)\n" `shouldBe` Right ["3"]

    it "returns 0 when start is past the string length" $ do
      runSourceVm "print 'hello'.count('', 6)\n" `shouldBe` Right ["0"]

    it "reports type error for a non-int end" $ do
      runSourceVm "print count('hello', 'l', 0, 'x')\n" `shouldBe` Left "Type error: count expects int start/end at 1:7"
