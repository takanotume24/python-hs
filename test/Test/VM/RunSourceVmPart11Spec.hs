module Test.VM.RunSourceVmPart11Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 11" $ do
  describe "split builtin" $ do
    it "splits on whitespace when no separator is given" $ do
      runSourceVm "print 'a b  c'.split()\n" `shouldBe` Right ["[a, b, c]"]

    it "ignores leading and trailing whitespace" $ do
      runSourceVm "print '  a b  '.split()\n" `shouldBe` Right ["[a, b]"]

    it "splits on an explicit separator" $ do
      runSourceVm "print 'a,b,c'.split(',')\n" `shouldBe` Right ["[a, b, c]"]

    it "keeps empty fields between separators" $ do
      runSourceVm "print 'a,,b'.split(',')\n" `shouldBe` Right ["[a, , b]"]

    it "returns one empty field for an empty string and separator" $ do
      runSourceVm "print ''.split(',')\n" `shouldBe` Right ["[]"]

    it "reports value error for an empty separator" $ do
      runSourceVm "print split('abc', '')\n" `shouldBe` Left "Value error: empty separator at 1:7"

    it "reports type error for a non-string receiver" $ do
      runSourceVm "print split(5)\n" `shouldBe` Left "Type error: split expects string at 1:7"

    it "reports type error for a non-string separator" $ do
      runSourceVm "print split('a', 5)\n" `shouldBe` Left "Type error: split expects string separator at 1:7"

    it "reports argument count mismatch when no argument" $ do
      runSourceVm "print split()\n" `shouldBe` Left "Argument count mismatch when calling split at 1:7"

    it "reports argument count mismatch when too many arguments" $ do
      runSourceVm "print split('a', ',', 1)\n" `shouldBe` Left "Argument count mismatch when calling split at 1:7"

  describe "join builtin" $ do
    it "joins a list of strings" $ do
      runSourceVm "print '-'.join(['a', 'b'])\n" `shouldBe` Right ["a-b"]

    it "joins a tuple of strings" $ do
      runSourceVm "print ''.join(('a', 'b', 'c'))\n" `shouldBe` Right ["abc"]

    it "returns an empty string for an empty sequence" $ do
      runSourceVm "print join(',', [])\n" `shouldBe` Right [""]

    it "reports type error for a non-string separator" $ do
      runSourceVm "print join(5, ['a'])\n" `shouldBe` Left "Type error: join expects string as first argument at 1:7"

    it "reports type error for a non-sequence" $ do
      runSourceVm "print join(',', 5)\n" `shouldBe` Left "Type error: join expects list or tuple as second argument at 1:7"

    it "reports type error for a non-string element" $ do
      runSourceVm "print join(',', ['a', 5])\n" `shouldBe` Left "Type error: join expects list of string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print join(',')\n" `shouldBe` Left "Argument count mismatch when calling join at 1:7"

  describe "lower builtin" $ do
    it "lowercases a string" $ do
      runSourceVm "print 'ABC'.lower()\n" `shouldBe` Right ["abc"]

    it "keeps lowercase characters unchanged" $ do
      runSourceVm "print lower('aBc')\n" `shouldBe` Right ["abc"]

    it "reports type error for a non-string" $ do
      runSourceVm "print lower(5)\n" `shouldBe` Left "Type error: lower expects string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print lower()\n" `shouldBe` Left "Argument count mismatch when calling lower at 1:7"

  describe "upper builtin" $ do
    it "uppercases a string" $ do
      runSourceVm "print 'abc'.upper()\n" `shouldBe` Right ["ABC"]

    it "keeps uppercase characters unchanged" $ do
      runSourceVm "print upper('AbC')\n" `shouldBe` Right ["ABC"]

    it "reports type error for a non-string" $ do
      runSourceVm "print upper(5)\n" `shouldBe` Left "Type error: upper expects string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print upper()\n" `shouldBe` Left "Argument count mismatch when calling upper at 1:7"

  describe "strip builtin" $ do
    it "strips surrounding whitespace" $ do
      runSourceVm "print '  x  '.strip()\n" `shouldBe` Right ["x"]

    it "strips the given characters from both ends" $ do
      runSourceVm "print 'xxhixx'.strip('x')\n" `shouldBe` Right ["hi"]

    it "reports type error for a non-string receiver" $ do
      runSourceVm "print strip(5)\n" `shouldBe` Left "Type error: strip expects string at 1:7"

    it "reports type error for non-string chars" $ do
      runSourceVm "print strip('x', 5)\n" `shouldBe` Left "Type error: strip expects string chars at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print strip()\n" `shouldBe` Left "Argument count mismatch when calling strip at 1:7"

  describe "replace builtin" $ do
    it "replaces every occurrence" $ do
      runSourceVm "print 'banana'.replace('a', 'o')\n" `shouldBe` Right ["bonono"]

    it "replaces non-overlapping occurrences left to right" $ do
      runSourceVm "print replace('aaa', 'aa', 'b')\n" `shouldBe` Right ["ba"]

    it "inserts the new value around every character for an empty old value" $ do
      runSourceVm "print replace('abc', '', '-')\n" `shouldBe` Right ["-a-b-c-"]

    it "returns the original string when the old value is absent" $ do
      runSourceVm "print replace('abc', 'x', 'y')\n" `shouldBe` Right ["abc"]

    it "reports type error for non-string arguments" $ do
      runSourceVm "print replace(5, 'a', 'b')\n" `shouldBe` Left "Type error: replace expects string arguments at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print replace('a', 'b')\n" `shouldBe` Left "Argument count mismatch when calling replace at 1:7"
