module Test.VM.RunSourceVmPart24Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 24" $ do
  describe "type builtin" $ do
    it "returns the builtin type names" $ do
      runSourceVm "print type(1)\nprint type(1.5)\nprint type('a')\nprint type(None)\n"
        `shouldBe` Right ["int", "float", "str", "NoneType"]

    it "returns collection type names" $ do
      runSourceVm "print type([])\nprint type((1, 2))\nprint type({})\n"
        `shouldBe` Right ["list", "tuple", "dict"]

    it "returns the user class name for instances" $ do
      runSourceVm "class User:\n  pass\nu = User()\nprint type(u)\n"
        `shouldBe` Right ["User"]

    it "returns type for a class object" $ do
      runSourceVm "class User:\n  pass\nprint type(User)\n"
        `shouldBe` Right ["type"]

    it "reports argument count mismatch" $ do
      runSourceVm "print type()\n" `shouldBe` Left "Argument count mismatch when calling type at 1:7"

  describe "isinstance builtin" $ do
    it "matches builtin type names" $ do
      runSourceVm "print isinstance(1, 'int')\nprint isinstance('a', 'int')\n"
        `shouldBe` Right ["1", "0"]

    it "matches the direct user class name" $ do
      runSourceVm "class User:\n  pass\nclass Other:\n  pass\nprint isinstance(User(), 'User')\nprint isinstance(User(), 'Other')\n"
        `shouldBe` Right ["1", "0"]

    it "works with type() output" $ do
      runSourceVm "class User:\n  pass\nprint isinstance(User(), type(User()))\n"
        `shouldBe` Right ["1"]

    it "reports a type error for a non-string class name" $ do
      runSourceVm "print isinstance(1, 5)\n"
        `shouldBe` Left "Type error: isinstance expects class name string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print isinstance(1)\n" `shouldBe` Left "Argument count mismatch when calling isinstance at 1:7"

  describe "hasattr builtin" $ do
    it "finds instance attributes" $ do
      runSourceVm "class User:\n  def __init__(self):\n    self.name = 'ann'\nu = User()\nprint hasattr(u, 'name')\nprint hasattr(u, 'age')\n"
        `shouldBe` Right ["1", "0"]

    it "finds class methods" $ do
      runSourceVm "class User:\n  def greet(self):\n    return 'hi'\nprint hasattr(User(), 'greet')\nprint hasattr(User(), 'missing')\n"
        `shouldBe` Right ["1", "0"]

    it "finds module attributes" $ do
      runSourceVm "import os.path\nprint hasattr(os, 'path')\nprint hasattr(os, 'missing')\n"
        `shouldBe` Right ["1", "0"]

    it "returns 0 for values without attributes" $ do
      runSourceVm "print hasattr(1, 'name')\n" `shouldBe` Right ["0"]

    it "reports a type error for a non-string attribute name" $ do
      runSourceVm "print hasattr(1, 5)\n"
        `shouldBe` Left "Type error: hasattr expects attribute name string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print hasattr(1)\n" `shouldBe` Left "Argument count mismatch when calling hasattr at 1:7"
