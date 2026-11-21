module Test.VM.RunSourceVmPart21Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 21" $ do
  describe "for loop tuple unpacking" $ do
    it "unpacks tuples into two names" $ do
      runSourceVm "for a, b in [(1, 2), (3, 4)]:\n    print a\n    print b\n"
        `shouldBe` Right ["1", "2", "3", "4"]

    it "unpacks the tuples produced by zip" $ do
      runSourceVm "for a, b in zip([1, 2], [3, 4]):\n    print a + b\n"
        `shouldBe` Right ["4", "6"]

    it "unpacks list elements" $ do
      runSourceVm "for a, b in [[1, 2], [3, 4]]:\n    print a * b\n"
        `shouldBe` Right ["2", "12"]

    it "supports three names" $ do
      runSourceVm "for a, b, c in [(1, 2, 3)]:\n    print a + b + c\n"
        `shouldBe` Right ["6"]

    it "keeps a single target working" $ do
      runSourceVm "for x in [1, 2]:\n    print x\n"
        `shouldBe` Right ["1", "2"]

    it "works inside a function body" $ do
      runSourceVm "def total():\n    result = 0\n    for a, b in [(1, 2), (3, 4)]:\n        result = result + a * b\n    return result\nprint total()\n"
        `shouldBe` Right ["14"]

    it "works with nested tuple unpacking loops" $ do
      runSourceVm "for a, b in [(1, 2)]:\n    for c, d in [(3, 4)]:\n        print a + b + c + d\n"
        `shouldBe` Right ["10"]

    it "reports a value error when the element arity does not match" $ do
      runSourceVm "for a, b in [(1, 2, 3)]:\n    print a\n"
        `shouldBe` Left "Value error: unpack mismatch at 1:1"

    it "reports a value error when unpacking a non-iterable element" $ do
      runSourceVm "for a, b in [1]:\n    print a\n"
        `shouldBe` Left "Value error: unpack mismatch at 1:1"
