{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.VM.RunSourceVmPart6Spec (spec) where

import Data.Either (isLeft)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 6" $ do
  it "reports dict-family builtin errors" $ do
    runSourceVm "print clear(1)\n" `shouldBe` Left "Type error: clear expects list or dict at 1:7"
    runSourceVm "print keys(1)\n" `shouldBe` Left "Type error: keys expects dict at 1:7"
    runSourceVm "print get([], 1)\n" `shouldBe` Left "Type error: get expects dict as first argument at 1:7"
    runSourceVm "print get({1: 10}, 2)\n" `shouldBe` Left "Key not found in get at 1:7"
    runSourceVm "print update({1: 10}, [])\n" `shouldBe` Left "Type error: update expects dict as second argument at 1:7"
    runSourceVm "print update([], {1: 10})\n" `shouldBe` Left "Type error: update expects dict as first argument at 1:7"
    runSourceVm "print setdefault([], 1)\n" `shouldBe` Left "Type error: setdefault expects dict as first argument at 1:7"
    runSourceVm "print values(1)\n" `shouldBe` Left "Type error: values expects dict at 1:7"
    runSourceVm "print items(1)\n" `shouldBe` Left "Type error: items expects dict at 1:7"

  describe "with statement" $ do
    it "executes with statement with context manager" $ do
      runSourceVm "class ContextManager:\n  def __enter__(self):\n    print 'enter'\n    return self\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit'\nwith ContextManager():\n  print 'body'" `shouldBe` Right ["enter", "body", "exit"]

    it "executes with statement with exception handling" $ do
      runSourceVm "class ContextManager:\n  def __enter__(self):\n    print 'enter'\n    return self\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit'\n    return 1\nwith ContextManager():\n  raise 'error'" `shouldBe` Right ["enter", "exit"]

    it "executes nested with statements" $ do
      runSourceVm "class ContextManager:\n  def __init__(self, name):\n    self.name = name\n  def __enter__(self):\n    print 'enter ' + self.name\n    return self\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit ' + self.name\nwith ContextManager('outer'):\n  with ContextManager('inner'):\n    print 'body'" `shouldBe` Right ["enter outer", "enter inner", "body", "exit inner", "exit outer"]

    it "executes with statement with __exit__ returning None" $ do
      runSourceVm "class ContextManager:\n  def __enter__(self):\n    print 'enter'\n    return self\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit'\nwith ContextManager():\n  raise 'error'" `shouldSatisfy` isLeft

    it "executes with statement with __exit__ returning truthy value" $ do
      runSourceVm "class ContextManager:\n  def __enter__(self):\n    print 'enter'\n    return self\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit'\n    return 1\nwith ContextManager():\n  raise 'error'" `shouldBe` Right ["enter", "exit"]

    it "executes with statement with __exit__ returning falsy value" $ do
      runSourceVm "class ContextManager:\n  def __enter__(self):\n    print 'enter'\n    return self\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit'\n    return 0\nwith ContextManager():\n  raise 'error'" `shouldSatisfy` isLeft

    it "handles simple context manager with print" $ do
      runSourceVm "class SimpleContext:\n  def __enter__(self):\n    return 'entered'\n  def __exit__(self, exc_type, exc_value, traceback):\n    return None\nwith SimpleContext() as ctx:\n  print ctx" `shouldBe` Right ["entered"]

    it "handles context manager with variable binding" $ do
      runSourceVm "class VarContext:\n  def __enter__(self):\n    return 'value'\n  def __exit__(self, exc_type, exc_value, traceback):\n    return None\nwith VarContext() as v:\n  print v" `shouldBe` Right ["value"]

    it "handles minimal context manager" $ do
      runSourceVm "class MinimalContext:\n  def __enter__(self):\n    pass\n  def __exit__(self, exc_type, exc_value, traceback):\n    pass\nwith MinimalContext():\n  pass" `shouldBe` Right []

    it "handles context manager with simple print" $ do
      runSourceVm "class PrintContext:\n  def __enter__(self):\n    print 'enter'\n  def __exit__(self, exc_type, exc_value, traceback):\n    print 'exit'\nwith PrintContext():\n  print 'body'" `shouldBe` Right ["enter", "body", "exit"]

    it "handles simplest successful with statement" $ do
      runSourceVm "class SimpleContext:\n  def __enter__(self):\n    return 'entered'\n  def __exit__(self, exc_type, exc_value, traceback):\n    return None\nresult = None\nwith SimpleContext() as ctx:\n  result = ctx\nprint result" `shouldBe` Right ["entered"]

    it "handles even simpler with statement" $ do
      runSourceVm "class SimpleContext:\n  def __enter__(self):\n    pass\n  def __exit__(self, exc_type, exc_value, traceback):\n    pass\nwith SimpleContext():\n  pass" `shouldBe` Right []

    it "handles simplest possible with statement" $ do
      runSourceVm "with None:\n  pass" `shouldSatisfy` isLeft

    it "handles minimal with statement" $ do
      runSourceVm "class MinimalContext:\n  def __enter__(self):\n    pass\n  def __exit__(self, exc_type, exc_value, traceback):\n    pass\nwith MinimalContext():\n  pass" `shouldBe` Right []

    it "handles simplest with statement" $ do
      runSourceVm "with None:\n  pass" `shouldSatisfy` isLeft

  describe "in / not in operators" $ do
    it "evaluates membership in list" $ do
      runSourceVm "print 1 in [1, 2, 3]\nprint 4 in [1, 2, 3]\n" `shouldBe` Right ["1", "0"]

    it "evaluates membership in dict keys" $ do
      runSourceVm "print 1 in {1: 10, 2: 20}\nprint 3 in {1: 10}\n" `shouldBe` Right ["1", "0"]

    it "evaluates membership in tuple" $ do
      runSourceVm "print 2 in (1, 2, 3)\nprint 5 in (1, 2)\n" `shouldBe` Right ["1", "0"]

    it "evaluates membership in string" $ do
      runSourceVm "print 'a' in 'abc'\nprint 'x' in 'abc'\n" `shouldBe` Right ["1", "0"]

    it "evaluates not in membership" $ do
      runSourceVm "print 1 not in [1, 2]\nprint 3 not in [1, 2]\n" `shouldBe` Right ["0", "1"]

    it "evaluates nested in expression" $ do
      runSourceVm "x = 1\nprint x in [1, 2]\n" `shouldBe` Right ["1"]

  describe "is / is not operators" $ do
    it "evaluates is operator with same integer value" $ do
      runSourceVm "print 1 is 1\nprint 1 is 2\n" `shouldBe` Right ["1", "0"]

    it "evaluates is not operator" $ do
      runSourceVm "print 1 is not 2\nprint 1 is not 1\n" `shouldBe` Right ["1", "0"]

    it "evaluates is with None" $ do
      runSourceVm "print None is None\nprint None is 0\n" `shouldBe` Right ["1", "0"]

    it "evaluates is with strings" $ do
      runSourceVm "print 'a' is 'a'\nprint 'a' is 'b'\n" `shouldBe` Right ["1", "0"]

  describe "bitwise operators" $ do
    it "evaluates bitwise and" $ do
      runSourceVm "print 5 & 3\n" `shouldBe` Right ["1"]

    it "evaluates bitwise or" $ do
      runSourceVm "print 5 | 3\n" `shouldBe` Right ["7"]

    it "evaluates bitwise xor" $ do
      runSourceVm "print 5 ^ 3\n" `shouldBe` Right ["6"]

    it "evaluates bitwise not" $ do
      runSourceVm "print ~5\nprint ~0\n" `shouldBe` Right ["-6", "-1"]

    it "evaluates left shift" $ do
      runSourceVm "print 1 << 3\n" `shouldBe` Right ["8"]

    it "evaluates right shift" $ do
      runSourceVm "print 8 >> 2\n" `shouldBe` Right ["2"]

    it "evaluates combined bitwise expression" $ do
      runSourceVm "print 1 | 2 & 2\nprint (1 | 2) & 2\n" `shouldBe` Right ["3", "2"]

    it "reports type error for float operand" $ do
      runSourceVm "print 1.0 & 2\n" `shouldBe` Left "Type error: & expects int at 1:11"

  describe "comparison chaining" $ do
    it "evaluates chained less-than" $ do
      runSourceVm "print 1 < 2 < 3\nprint 1 < 3 < 2\n" `shouldBe` Right ["1", "0"]

    it "evaluates chained mixed comparisons" $ do
      runSourceVm "print 1 < 3 > 2\nprint 1 <= 2 <= 2\n" `shouldBe` Right ["1", "1"]

    it "short-circuits chained comparison" $ do
      runSourceVm "def f():\n    print 'evaluated'\n    return 5\nprint 1 < 2 < f()\n" `shouldBe` Right ["evaluated", "1"]

    it "short-circuits chained comparison on first false" $ do
      runSourceVm "def f():\n    print 'evaluated'\n    return 5\nprint 3 < 2 < f()\n" `shouldBe` Right ["0"]

  describe "function definition varargs" $ do
    it "prints *args directly" $ do
      runSourceVm "def f(*args):\n  print args\nf(1, 2, 3)\n" `shouldBe` Right ["[1, 2, 3]"]

    it "prints **kwargs directly" $ do
      runSourceVm "def f(**kwargs):\n  print kwargs\nf(x=7, y=8)\n" `shouldBe` Right ["{x: 7, y: 8}"]

    it "combines *args and **kwargs in function definition" $ do
      runSourceVm "def f(*args, **kwargs):\n  return len(args) + get(kwargs, \"x\", 0)\nprint f(1, 2, x=3)\n" `shouldBe` Right ["5"]

    it "prints *args after a positional parameter" $ do
      runSourceVm "def f(a, *args):\n  print args\nf(1, 2, 3)\n" `shouldBe` Right ["[2, 3]"]

    it "prints **kwargs after a positional parameter" $ do
      runSourceVm "def f(a, **kwargs):\n  print kwargs\nf(1, x=7)\n" `shouldBe` Right ["{x: 7}"]

    it "rejects positional argument for **kwargs-only function" $ do
      runSourceVm "def f(**kwargs):\n  pass\nf(1)\n" `shouldBe` Left "Argument count mismatch when calling f at 3:1"

    it "rejects keyword argument for *args-only function" $ do
      runSourceVm "def f(*args):\n  pass\nf(a=1)\n" `shouldBe` Left "Argument error: unexpected keyword argument a at 3:3"

    it "rejects keyword-only arguments after *args as unsupported" $ do
      runSourceVm "def f(*args, b):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 14})"

  describe "walrus operator" $ do
    it "evaluates walrus operator in print" $ do
      runSourceVm "print (x := 5)\nprint x\n" `shouldBe` Right ["5", "5"]

    it "evaluates walrus operator in if condition" $ do
      runSourceVm "if (n := 1):\n  print n\n" `shouldBe` Right ["1"]

    it "evaluates walrus operator in list comprehension condition" $ do
      runSourceVm "print [x for x in [1, 2, 3, 4] if (y := x % 2) == 0]\n" `shouldBe` Right ["[2, 4]"]

    it "evaluates walrus operator in while condition" $ do
      runSourceVm "i = 0\nwhile (n := i) < 3:\n  print n\n  i = i + 1\n" `shouldBe` Right ["0", "1", "2"]

    it "evaluates walrus operator inside arithmetic expression" $ do
      runSourceVm "print (x := 2) + 1\nprint x\n" `shouldBe` Right ["3", "2"]

    it "keeps walrus binding local to list comprehension" $ do
      runSourceVm "print [x for x in [1, 2] if (y := x) == 1]\nprint y\n" `shouldBe` Left "Name error: undefined identifier y at 2:7"
