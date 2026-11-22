module Test.VM.RunSourceVmPart23Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 23" $ do
  describe "__str__ and __repr__" $ do
    it "uses __str__ for print" $ do
      runSourceVm "class User:\n  def __init__(self, name):\n    self.name = name\n  def __str__(self):\n    return 'User(' + self.name + ')'\nu = User('ann')\nprint u\n"
        `shouldBe` Right ["User(ann)"]

    it "uses __str__ for the str() builtin" $ do
      runSourceVm "class User:\n  def __init__(self, name):\n    self.name = name\n  def __str__(self):\n    return 'User(' + self.name + ')'\nu = User('ann')\nprint str(u)\n"
        `shouldBe` Right ["User(ann)"]

    it "falls back to __repr__ when __str__ is absent" $ do
      runSourceVm "class User:\n  def __repr__(self):\n    return 'User()'\nu = User()\nprint u\n"
        `shouldBe` Right ["User()"]

    it "prefers __str__ over __repr__" $ do
      runSourceVm "class User:\n  def __str__(self):\n    return 'str'\n  def __repr__(self):\n    return 'repr'\nu = User()\nprint u\nprint str(u)\n"
        `shouldBe` Right ["str", "str"]

    it "keeps the default instance representation without either method" $ do
      runSourceVm "class Empty:\n  pass\ne = Empty()\nprint e\n"
        `shouldBe` Right ["<Empty instance>"]

    it "inherits __str__ from a base class" $ do
      runSourceVm "class Base:\n  def __str__(self):\n    return 'base'\nclass Child(Base):\n  pass\nprint Child()\n"
        `shouldBe` Right ["base"]

    it "keeps outputs produced inside __str__" $ do
      runSourceVm "class Noisy:\n  def __str__(self):\n    print 'inside'\n    return 'noisy'\nprint Noisy()\n"
        `shouldBe` Right ["inside", "noisy"]

    it "propagates errors raised inside __str__" $ do
      runSourceVm "class Bad:\n  def __str__(self):\n    raise ValueError('boom')\nprint Bad()\n"
        `shouldBe` Left "Value error: boom at 3:5"

  describe "dataclass __repr__ consistency" $ do
    it "prints dataclass instances using the generated __repr__" $ do
      runSourceVm "from dataclasses import dataclass\n@dataclass\nclass Point:\n  x: int\n  y: int\nprint Point(1, 2)\n"
        `shouldBe` Right ["Point(x=1, y=2)"]

    it "keeps str() consistent with print for dataclasses" $ do
      runSourceVm "from dataclasses import dataclass\n@dataclass\nclass Point:\n  x: int\n  y: int\nprint str(Point(1, 2))\n"
        `shouldBe` Right ["Point(x=1, y=2)"]
