module Test.VM.RunSourceVmPart40Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm set support" $ do
  describe "set literals and constructors" $ do
    it "builds a set literal and removes duplicates" $ do
      runSourceVm "print {1, 2, 2, 3}\n"
        `shouldBe` Right ["{1, 2, 3}"]

    it "builds a set from an iterable" $ do
      runSourceVm "print set([1, 2, 2])\n"
        `shouldBe` Right ["{1, 2}"]

    it "builds a frozenset from an iterable" $ do
      runSourceVm "print frozenset([1, 2, 2])\n"
        `shouldBe` Right ["frozenset({1, 2})"]

    it "keeps an empty dict distinct from an empty set" $ do
      runSourceVm "print {}\nprint set()\n"
        `shouldBe` Right ["{}", "set()"]

  describe "set protocol" $ do
    it "reports length and membership" $ do
      runSourceVm "s = {1, 2, 3}\nprint len(s)\nprint 2 in s\nprint 4 in s\n"
        `shouldBe` Right ["3", "1", "0"]

    it "reports truthiness" $ do
      runSourceVm "print bool(set())\nprint bool({1})\n"
        `shouldBe` Right ["0", "1"]

    it "iterates in insertion order" $ do
      runSourceVm "for x in {3, 1, 2}:\n    print x\n"
        `shouldBe` Right ["3", "1", "2"]

    it "compares sets regardless of order" $ do
      runSourceVm "print {1, 2} == {2, 1}\nprint {1, 2} == {1, 3}\n"
        `shouldBe` Right ["1", "0"]

  describe "set operations" $ do
    it "adds an element functionally" $ do
      runSourceVm "s = add({1}, 2)\nprint s\n"
        `shouldBe` Right ["{1, 2}"]

    it "removes an element functionally" $ do
      runSourceVm "print remove({1, 2}, 1)\n"
        `shouldBe` Right ["{2}"]

    it "discards a missing element without an error" $ do
      runSourceVm "print discard({1, 2}, 9)\n"
        `shouldBe` Right ["{1, 2}"]

    it "updates a set with another iterable" $ do
      runSourceVm "print update({1}, [2, 2, 3])\n"
        `shouldBe` Right ["{1, 2, 3}"]

    it "clears a set" $ do
      runSourceVm "print clear({1, 2})\n"
        `shouldBe` Right ["set()"]

    it "pops an element from a set" $ do
      runSourceVm "print pop({7})\n"
        `shouldBe` Right ["7"]

    it "computes union, intersection, and difference" $ do
      runSourceVm "print union({1, 2}, {2, 3})\nprint intersection({1, 2}, {2, 3})\nprint difference({1, 2}, {2, 3})\n"
        `shouldBe` Right ["{1, 2, 3}", "{2}", "{1}"]

    it "supports set operators" $ do
      runSourceVm "print {1, 2} | {2, 3}\nprint {1, 2} & {2, 3}\nprint {1, 2} - {2, 3}\n"
        `shouldBe` Right ["{1, 2, 3}", "{2}", "{1}"]

    it "builds a set comprehension" $ do
      runSourceVm "print {x for x in [1, 2, 2, 3] if x > 1}\n"
        `shouldBe` Right ["{2, 3}"]
