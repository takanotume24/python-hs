module Test.VM.RunSourceVmPart19Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 19" $ do
  describe "map" $ do
    it "maps a user-defined function over a list" $ do
      runSourceVm "def double(x):\n  return x * 2\nprint map(double, [1, 2, 3])\n" `shouldBe` Right ["[2, 4, 6]"]

    it "maps a lambda over a list" $ do
      runSourceVm "print map(lambda x: x + 1, [1, 2])\n" `shouldBe` Right ["[2, 3]"]

    it "maps over a tuple" $ do
      runSourceVm "print map(lambda x: x * 10, (1, 2))\n" `shouldBe` Right ["[10, 20]"]

    it "maps over a string" $ do
      runSourceVm "print map(lambda c: c + '!', 'ab')\n" `shouldBe` Right ["[a!, b!]"]

    it "maps over dictionary keys" $ do
      runSourceVm "print map(lambda k: k + 1, {1: 'a', 2: 'b'})\n" `shouldBe` Right ["[2, 3]"]

    it "keeps an empty iterable empty" $ do
      runSourceVm "print map(lambda x: x, [])\n" `shouldBe` Right ["[]"]

    it "runs the function for each item and preserves order" $ do
      runSourceVm "def note(x):\n  print x\n  return x\nprint map(note, [1, 2])\n" `shouldBe` Right ["1", "2", "[1, 2]"]

    it "supports nested map calls" $ do
      runSourceVm "print map(lambda x: x + 1, map(lambda x: x * 2, [1, 2]))\n" `shouldBe` Right ["[3, 5]"]

    it "captures outer variables in a lambda" $ do
      runSourceVm "def makeAdder(n):\n  return lambda x: x + n\nprint map(makeAdder(10), [1, 2])\n" `shouldBe` Right ["[11, 12]"]

  describe "filter" $ do
    it "filters a list with a user-defined function" $ do
      runSourceVm "def even(x):\n  return x % 2 == 0\nprint filter(even, [1, 2, 3, 4])\n" `shouldBe` Right ["[2, 4]"]

    it "filters a tuple with a lambda" $ do
      runSourceVm "print filter(lambda x: x > 1, (1, 2, 3))\n" `shouldBe` Right ["[2, 3]"]

    it "filters a string" $ do
      runSourceVm "print filter(lambda c: c == 'a', 'aba')\n" `shouldBe` Right ["[a, a]"]

    it "filters dictionary keys" $ do
      runSourceVm "print filter(lambda k: k > 1, {1: 'a', 2: 'b'})\n" `shouldBe` Right ["[2]"]

    it "uses truthiness of the predicate result" $ do
      runSourceVm "print filter(lambda x: x, [0, 1, '', 'x', [], [1]])\n" `shouldBe` Right ["[1, x, [1]]"]

    it "keeps an empty iterable empty" $ do
      runSourceVm "print filter(lambda x: x, ())\n" `shouldBe` Right ["[]"]

  describe "name resolution" $ do
    it "prefers a user-defined function named map" $ do
      runSourceVm "def map(x):\n  return x + 1\nprint map(1)\n" `shouldBe` Right ["2"]

    it "prefers a class method named map" $ do
      runSourceVm "class C:\n  def map(self, x):\n    return x + 1\nc = C()\nprint c.map(2)\n" `shouldBe` Right ["3"]

    it "prefers a class method named filter" $ do
      runSourceVm "class C:\n  def filter(self, x):\n    return x * 2\nc = C()\nprint c.filter(3)\n" `shouldBe` Right ["6"]

  describe "errors" $ do
    it "rejects a non-iterable for map" $ do
      runSourceVm "print map(lambda x: x, 5)\n" `shouldBe` Left "Type error: map expects iterable at 1:7"

    it "rejects a non-iterable for filter" $ do
      runSourceVm "print filter(lambda x: x, 5)\n" `shouldBe` Left "Type error: filter expects iterable at 1:7"

    it "rejects a non-callable for map" $ do
      runSourceVm "print map(1, [1])\n" `shouldBe` Left "Type error: callable expected at 1:7"

    it "rejects a wrong argument count for map" $ do
      runSourceVm "print map(lambda x: x)\n" `shouldBe` Left "Argument count mismatch when calling map at 1:7"

    it "rejects a wrong argument count for filter" $ do
      runSourceVm "print filter(lambda x: x, [1], [2])\n" `shouldBe` Left "Argument count mismatch when calling filter at 1:7"
