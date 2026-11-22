module Test.VM.RunSourceVmPart28Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 28" $ do
  describe "call-site * expansion" $ do
    it "expands a list into positional arguments" $ do
      runSourceVm "def f(a, b):\n  return a + b\nprint f(*[1, 2])\n" `shouldBe` Right ["3"]

    it "expands a tuple" $ do
      runSourceVm "def f(a, b):\n  return a + b\nprint f(*(1, 2))\n" `shouldBe` Right ["3"]

    it "expands a string into characters" $ do
      runSourceVm "def f(a, b):\n  return a + b\nprint f(*'ab')\n" `shouldBe` Right ["ab"]

    it "expands dictionary keys" $ do
      runSourceVm "def f(a, b):\n  return a + b\nd = {'x': 1, 'y': 2}\nprint f(*d)\n" `shouldBe` Right ["xy"]

    it "combines explicit and expanded positionals" $ do
      runSourceVm "def f(a, b, c):\n  return a + b + c\nprint f(1, *[2, 3])\n" `shouldBe` Right ["6"]

    it "rejects a non-iterable expansion" $ do
      runSourceVm "def f(a):\n  pass\nf(*1)\n" `shouldBe` Left "Type error: * expects iterable at 3:3"

  describe "call-site ** expansion" $ do
    it "expands a dict into keyword arguments" $ do
      runSourceVm "def f(a, b):\n  return a + b\nprint f(**{'a': 1, 'b': 2})\n" `shouldBe` Right ["3"]

    it "combines explicit and expanded keyword arguments" $ do
      runSourceVm "def f(a, b):\n  return a + b\nprint f(a=1, **{'b': 2})\n" `shouldBe` Right ["3"]

    it "rejects a non-dict expansion" $ do
      runSourceVm "def f(a):\n  pass\nf(**1)\n" `shouldBe` Left "Type error: ** expects dict at 3:3"

    it "rejects a duplicate keyword" $ do
      runSourceVm "def f(a):\n  pass\nf(a=1, **{'a': 2})\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:8"

    it "rejects non-string keyword keys" $ do
      runSourceVm "def f(a):\n  pass\nf(**{1: 2})\n" `shouldBe` Left "Type error: keyword argument keys must be strings at 3:3"

  describe "definition *args/**kwargs with defaults" $ do
    it "binds defaults before varargs" $ do
      runSourceVm "def f(a, b=10, *args):\n  return a + b + len(args)\nprint f(1)\nprint f(1, 2)\nprint f(1, 2, 3, 4)\n"
        `shouldBe` Right ["11", "3", "5"]

    it "binds varargs and kwargs after a default" $ do
      runSourceVm "def f(a=1, *args, **kwargs):\n  return a + len(args) + len(kwargs)\nprint f(2, 3, 4, x=5, y=6)\n"
        `shouldBe` Right ["6"]

    it "binds positional, default and keyword arguments together" $ do
      runSourceVm "def f(a, b=2, **kwargs):\n  return a + b + get(kwargs, 'c', 0)\nprint f(1, c=3)\nprint f(1, 5, c=3)\n"
        `shouldBe` Right ["6", "9"]

    it "passes varargs through a nested call" $ do
      runSourceVm "def g(*args):\n  return len(args)\ndef f(*args):\n  return g(*args)\nprint f(1, 2, 3)\n"
        `shouldBe` Right ["3"]

    it "passes kwargs through a nested call" $ do
      runSourceVm "def g(**kwargs):\n  return get(kwargs, 'x', 0)\ndef f(**kwargs):\n  return g(**kwargs)\nprint f(x=4)\n"
        `shouldBe` Right ["4"]

  describe "regression" $ do
    it "keeps normal positional and keyword calls working" $ do
      runSourceVm "def f(a, b=2):\n  return a + b\nprint f(1)\nprint f(1, 5)\nprint f(a=3, b=4)\n"
        `shouldBe` Right ["3", "6", "7"]
