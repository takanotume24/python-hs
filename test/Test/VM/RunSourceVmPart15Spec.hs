module Test.VM.RunSourceVmPart15Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 15" $ do
  describe "sorted builtin with strings" $ do
    it "sorts a list of strings" $ do
      runSourceVm "print sorted(['b', 'a', 'c'])\n" `shouldBe` Right ["[a, b, c]"]

    it "sorts a tuple of strings into a list" $ do
      runSourceVm "print sorted(('b', 'a'))\n" `shouldBe` Right ["[a, b]"]

    it "uses lexicographic order" $ do
      runSourceVm "print sorted(['B', 'a', 'b'])\n" `shouldBe` Right ["[B, a, b]"]

    it "reports type error for mixed numbers and strings" $ do
      runSourceVm "print sorted([1, 'a'])\n" `shouldBe` Left "Type error: sorted expects list of number or string at 1:7"

  describe "sort builtin with strings" $ do
    it "sorts a list of strings" $ do
      runSourceVm "print sort(['b', 'a'])\n" `shouldBe` Right ["[a, b]"]

    it "sorts a list in place when reassigned" $ do
      runSourceVm "xs = ['b', 'a']\nxs = sort(xs)\nprint xs\n" `shouldBe` Right ["[a, b]"]

    it "reports type error for mixed numbers and strings" $ do
      runSourceVm "print sort([1, 'a'])\n" `shouldBe` Left "Type error: sort expects list of number or string at 1:7"
