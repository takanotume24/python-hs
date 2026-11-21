module Test.VM.RunSourceVmPart12Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 12" $ do
  describe "startswith builtin" $ do
    it "returns 1 when the string starts with the prefix" $ do
      runSourceVm "print 'hello'.startswith('he')\n" `shouldBe` Right ["1"]

    it "returns 0 when the string does not start with the prefix" $ do
      runSourceVm "print 'hello'.startswith('lo')\n" `shouldBe` Right ["0"]

    it "treats an empty prefix as a match" $ do
      runSourceVm "print startswith('hello', '')\n" `shouldBe` Right ["1"]

    it "reports type error for a non-string receiver" $ do
      runSourceVm "print startswith(5, 'a')\n" `shouldBe` Left "Type error: startswith expects string at 1:7"

    it "reports type error for a non-string prefix" $ do
      runSourceVm "print startswith('a', 5)\n" `shouldBe` Left "Type error: startswith expects string arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print startswith('a')\n" `shouldBe` Left "Argument count mismatch when calling startswith at 1:7"

  describe "endswith builtin" $ do
    it "returns 1 when the string ends with the suffix" $ do
      runSourceVm "print 'hello'.endswith('lo')\n" `shouldBe` Right ["1"]

    it "returns 0 when the string does not end with the suffix" $ do
      runSourceVm "print 'hello'.endswith('he')\n" `shouldBe` Right ["0"]

    it "treats an empty suffix as a match" $ do
      runSourceVm "print endswith('hello', '')\n" `shouldBe` Right ["1"]

    it "reports type error for a non-string receiver" $ do
      runSourceVm "print endswith(5, 'a')\n" `shouldBe` Left "Type error: endswith expects string at 1:7"

    it "reports type error for a non-string suffix" $ do
      runSourceVm "print endswith('a', 5)\n" `shouldBe` Left "Type error: endswith expects string arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print endswith('a')\n" `shouldBe` Left "Argument count mismatch when calling endswith at 1:7"

  describe "find builtin" $ do
    it "returns the first index of the substring" $ do
      runSourceVm "print 'hello'.find('l')\n" `shouldBe` Right ["2"]

    it "returns -1 when the substring is absent" $ do
      runSourceVm "print 'hello'.find('z')\n" `shouldBe` Right ["-1"]

    it "returns 0 for an empty substring" $ do
      runSourceVm "print find('abc', '')\n" `shouldBe` Right ["0"]

    it "finds a substring at a later index" $ do
      runSourceVm "print find('abc', 'bc')\n" `shouldBe` Right ["1"]

    it "reports type error for a non-string receiver" $ do
      runSourceVm "print find(5, 'a')\n" `shouldBe` Left "Type error: find expects string at 1:7"

    it "reports type error for a non-string substring" $ do
      runSourceVm "print find('a', 5)\n" `shouldBe` Left "Type error: find expects string arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print find('a')\n" `shouldBe` Left "Argument count mismatch when calling find at 1:7"

  describe "count builtin" $ do
    it "counts non-overlapping occurrences" $ do
      runSourceVm "print 'banana'.count('a')\n" `shouldBe` Right ["3"]

    it "does not count overlapping occurrences" $ do
      runSourceVm "print 'aaa'.count('aa')\n" `shouldBe` Right ["1"]

    it "counts an empty substring as length plus one" $ do
      runSourceVm "print count('abc', '')\n" `shouldBe` Right ["4"]

    it "returns 0 when the substring is absent" $ do
      runSourceVm "print count('abc', 'x')\n" `shouldBe` Right ["0"]

    it "reports type error for a non-string receiver" $ do
      runSourceVm "print count(5, 'a')\n" `shouldBe` Left "Type error: count expects string at 1:7"

    it "reports type error for a non-string substring" $ do
      runSourceVm "print count('a', 5)\n" `shouldBe` Left "Type error: count expects string arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print count('a')\n" `shouldBe` Left "Argument count mismatch when calling count at 1:7"
