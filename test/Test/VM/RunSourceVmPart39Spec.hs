module Test.VM.RunSourceVmPart39Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm statement compatibility" $ do
  describe "conditional expressions" $ do
    it "chooses the then branch when the condition is true" $ do
      runSourceVm "print 1 if True else 2\n"
        `shouldBe` Right ["1"]

    it "chooses the else branch when the condition is false" $ do
      runSourceVm "print 1 if False else 2\n"
        `shouldBe` Right ["2"]

    it "nests conditional expressions to the right" $ do
      runSourceVm "x = 0\nprint 1 if x == 1 else 2 if x == 0 else 3\n"
        `shouldBe` Right ["2"]

    it "evaluates the condition only once" $ do
      runSourceVm "def f():\n    print 'cond'\n    return True\nprint 'yes' if f() else 'no'\n"
        `shouldBe` Right ["cond", "yes"]

  describe "loop else clauses" $ do
    it "runs the for-else body after normal completion" $ do
      runSourceVm "for i in [1, 2]:\n    print i\nelse:\n    print 'done'\n"
        `shouldBe` Right ["1", "2", "done"]

    it "skips the for-else body after a break" $ do
      runSourceVm "for i in [1, 2, 3]:\n    if i == 2:\n        break\n    print i\nelse:\n    print 'done'\n"
        `shouldBe` Right ["1"]

    it "runs the while-else body after normal completion" $ do
      runSourceVm "i = 0\nwhile i < 2:\n    print i\n    i = i + 1\nelse:\n    print 'done'\n"
        `shouldBe` Right ["0", "1", "done"]

    it "skips the while-else body after a break" $ do
      runSourceVm "i = 0\nwhile i < 3:\n    if i == 1:\n        break\n    print i\n    i = i + 1\nelse:\n    print 'done'\n"
        `shouldBe` Right ["0"]

  describe "starred displays" $ do
    it "expands a starred list item" $ do
      runSourceVm "a = [1, 2]\nprint [0, *a, 3]\n"
        `shouldBe` Right ["[0, 1, 2, 3]"]

    it "expands multiple starred list items" $ do
      runSourceVm "a = [1]\nb = [2, 3]\nprint [*a, *b]\n"
        `shouldBe` Right ["[1, 2, 3]"]

    it "expands a starred tuple item" $ do
      runSourceVm "a = [1, 2]\nprint (*a, 3)\n"
        `shouldBe` Right ["(1, 2, 3)"]

    it "expands a starred dict entry" $ do
      runSourceVm "a = {1: 2}\nprint {**a, 3: 4}\n"
        `shouldBe` Right ["{1: 2, 3: 4}"]

    it "lets a later dict entry override a spread key" $ do
      runSourceVm "a = {1: 2}\nprint {**a, 1: 9}\n"
        `shouldBe` Right ["{1: 9}"]

  describe "raise from" $ do
    it "accepts a cause expression" $ do
      runSourceVm "try:\n    raise ValueError('x') from KeyError('y')\nexcept ValueError:\n    print 1\n"
        `shouldBe` Right ["1"]

  describe "class annotations" $ do
    it "stores an annotated class attribute" $ do
      runSourceVm "class A:\n    x: int = 1\nprint A.x\n"
        `shouldBe` Right ["1"]

    it "inherits an annotated class attribute" $ do
      runSourceVm "class A:\n    x: int = 1\nclass B(A):\n    pass\nprint B.x\n"
        `shouldBe` Right ["1"]

  describe "super" $ do
    it "calls the base class method through super()" $ do
      runSourceVm "class A:\n    def greet(self):\n        return 'A'\nclass B(A):\n    def greet(self):\n        return super().greet() + 'B'\nprint B().greet()\n"
        `shouldBe` Right ["AB"]
