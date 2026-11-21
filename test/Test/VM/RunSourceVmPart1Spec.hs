{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.VM.RunSourceVmPart1Spec (spec) where

import Data.Either (isLeft)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 1" $ do
  it "raises runtime error from raise expression" $ do
    runSourceVm "raise \"boom\"\n" `shouldBe` Left "Runtime error: boom at 1:1"

  it "catches runtime error with try/except and continues" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept:\n  print 1\nprint 2\n" `shouldBe` Right ["1", "2"]

  it "catches non-raise runtime error with try/except and continues" $ do
    runSourceVm "try:\n  print 1 / 0\nexcept:\n  print 7\nprint 8\n" `shouldBe` Right ["7", "8"]

  it "skips except block when try body succeeds" $ do
    runSourceVm "try:\n  print 3\nexcept:\n  print 9\n" `shouldBe` Right ["3"]

  it "runs finally when try body succeeds" $ do
    runSourceVm "try:\n  print 1\nexcept:\n  print 9\nfinally:\n  print 2\n" `shouldBe` Right ["1", "2"]

  it "runs finally after except catches an error" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept:\n  print 7\nfinally:\n  print 8\n" `shouldBe` Right ["7", "8"]

  it "runs only the first except suite when multiple except suites exist" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept:\n  print 1\nexcept:\n  print 2\nfinally:\n  print 3\n" `shouldBe` Right ["1", "3"]

  it "matches typed except by runtime error category" $ do
    runSourceVm "try:\n  print len(1)\nexcept NameError:\n  print 1\nexcept TypeError:\n  print 2\nexcept:\n  print 3\n" `shouldBe` Right ["2"]

  it "matches Exception as catch-all and binds raised value" $ do
    runSourceVm "try:\n  raise 1\nexcept Exception as e:\n  result = e\nprint result\n" `shouldBe` Right ["1"]

  it "matches BaseException as catch-all and binds raised value" $ do
    runSourceVm "try:\n  raise 1\nexcept BaseException as e:\n  result = e\nprint result\n" `shouldBe` Right ["1"]

  it "supports top-level expression statements" $ do
    runSourceVm "def f():\n  print 1\nf()\n" `shouldBe` Right ["1"]

  it "fails top-level assert statement" $ do
    runSourceVm "assert False\n" `shouldBe` Left "AssertionError: assertion failed at 1:1"

  it "passes top-level assert statement" $ do
    runSourceVm "assert True\n" `shouldBe` Right []

  it "catches AssertionError" $ do
    runSourceVm "try:\n  assert False\nexcept AssertionError:\n  print 1\n" `shouldBe` Right ["1"]

  it "supports nested function closing over outer local variable" $ do
    runSourceVm "def outer():\n  x = 1\n  def inner():\n    return x\n  return inner\nr = outer()\nprint r()\n" `shouldBe` Right ["1"]

  it "matches AttributeError category" $ do
    runSourceVm "class A:\n  pass\na = A()\ntry:\n  _ = a.missing\nexcept AttributeError:\n  print 1\n" `shouldBe` Right ["1"]

  it "binds typed except alias to raised string value and rethrows on non-matching type" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept RuntimeError as e:\n  print e\n" `shouldBe` Right ["boom"]
    runSourceVm "try:\n  raise \"boom\"\nexcept TypeError:\n  print 1\n" `shouldBe` Left "Runtime error: boom at 2:3"

  it "matches one of parenthesized except types" $ do
    runSourceVm "try:\n  raise 1\nexcept (ValueError, RuntimeError):\n  print 11\n" `shouldBe` Right ["11"]

  it "binds alias to raised string value with parenthesized except types" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept (ValueError, RuntimeError) as e:\n  print e\n" `shouldBe` Right ["boom"]

  it "rethrows when no parenthesized except type matches" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept (ValueError, NameError):\n  print 11\n" `shouldBe` Left "Runtime error: boom at 2:3"

  it "matches typed suites in order before catch-all" $ do
    runSourceVm "try:\n  raise 1\nexcept NameError:\n  print 10\nexcept (TypeError, RuntimeError):\n  print 11\nexcept:\n  print 12\n" `shouldBe` Right ["11"]

  it "runs else suite when try body succeeds" $ do
    runSourceVm "try:\n  print 1\nexcept:\n  print 9\nelse:\n  print 2\n" `shouldBe` Right ["1", "2"]

  it "skips else suite when except catches an error" $ do
    runSourceVm "try:\n  raise \"boom\"\nexcept:\n  print 7\nelse:\n  print 8\n" `shouldBe` Right ["7"]

  it "runs else suite before finally when try body succeeds" $ do
    runSourceVm "try:\n  print 1\nexcept:\n  print 9\nelse:\n  print 2\nfinally:\n  print 3\n" `shouldBe` Right ["1", "2", "3"]

  it "propagates error raised in else suite" $ do
    runSourceVm "try:\n  print 1\nexcept:\n  print 9\nelse:\n  raise \"late\"\n" `shouldBe` Left "Runtime error: late at 6:3"

  it "raises builtin exception class with message" $ do
    runSourceVm "raise ValueError(\"boom\")\n" `shouldBe` Left "Value error: boom at 1:1"

  it "catches builtin ValueError with except ValueError" $ do
    runSourceVm "try:\n  raise ValueError(\"boom\")\nexcept ValueError:\n  print 1\nprint 2\n" `shouldBe` Right ["1", "2"]

  it "propagates when except TypeError does not match builtin ValueError" $ do
    runSourceVm "try:\n  raise ValueError(\"boom\")\nexcept TypeError:\n  print 1\n" `shouldBe` Left "Value error: boom at 2:3"

  it "raises builtin TypeError with message" $ do
    runSourceVm "raise TypeError(\"bad\")\n" `shouldBe` Left "Type error: bad at 1:1"

  it "raises builtin NameError with message" $ do
    runSourceVm "raise NameError(\"missing\")\n" `shouldBe` Left "Name error: missing at 1:1"

  it "raises builtin AttributeError with message" $ do
    runSourceVm "raise AttributeError(\"no attr\")\n" `shouldBe` Left "Attribute error: no attr at 1:1"

  it "catches builtin AttributeError with except AttributeError" $ do
    runSourceVm "try:\n  raise AttributeError(\"no attr\")\nexcept AttributeError:\n  print 1\n" `shouldBe` Right ["1"]

  it "catches builtin ValueError with except Exception" $ do
    runSourceVm "try:\n  raise ValueError(\"boom\")\nexcept Exception:\n  print 1\n" `shouldBe` Right ["1"]

  it "catches builtin TypeError with except BaseException" $ do
    runSourceVm "try:\n  raise TypeError(\"bad\")\nexcept BaseException:\n  print 1\n" `shouldBe` Right ["1"]

  it "binds builtin exception to alias with except ... as" $ do
    runSourceVm "try:\n  raise ValueError(\"boom\")\nexcept ValueError as e:\n  print e\n" `shouldBe` Right ["boom"]

  it "re-raises current exception with bare raise" $ do
    runSourceVm "try:\n  raise ValueError(\"boom\")\nexcept:\n  raise\n" `shouldBe` Left "Value error: boom at 2:3"

  it "raises user-defined exception class with message attribute" $ do
    runSourceVm "class MyError:\n  def __init__(self, message):\n    self.message = message\nraise MyError(\"boom\")\n" `shouldBe` Left "MyError: boom at 4:1"

  it "catches user-defined exception class with except" $ do
    runSourceVm "class MyError:\n  def __init__(self, message):\n    self.message = message\ntry:\n  raise MyError(\"boom\")\nexcept MyError:\n  print 1\n" `shouldBe` Right ["1"]

  it "catches user-defined exception class with except Exception as" $ do
    runSourceVm "class MyError:\n  def __init__(self, message):\n    self.message = message\ntry:\n  raise MyError(\"boom\")\nexcept Exception as e:\n  print e\n" `shouldBe` Right ["boom"]

  it "catches user-defined exception class with except BaseException as" $ do
    runSourceVm "class MyError:\n  def __init__(self, message):\n    self.message = message\ntry:\n  raise MyError(\"boom\")\nexcept BaseException as e:\n  print e\n" `shouldBe` Right ["boom"]

  it "catches user-defined exception subclass with except builtin base class" $ do
    runSourceVm "class MyError(ValueError):\n  def __init__(self, message):\n    self.message = message\ntry:\n  raise MyError(\"boom\")\nexcept ValueError:\n  print 1\n" `shouldBe` Right ["1"]

  it "propagates user-defined exception subclass when except base class does not match" $ do
    runSourceVm "class MyError(ValueError):\n  def __init__(self, message):\n    self.message = message\ntry:\n  raise MyError(\"boom\")\nexcept TypeError:\n  print 1\n" `shouldBe` Left "MyError: boom at 5:3"

  it "replaces original exception when finally raises" $ do
    runSourceVm "try:\n  raise 1\nfinally:\n  raise 2\n" `shouldBe` Left "Runtime error: 2 at 4:3"

  it "raises in finally when try body succeeds" $ do
    runSourceVm "try:\n  print 1\nfinally:\n  raise 2\n" `shouldBe` Left "Runtime error: 2 at 4:3"

  it "runs finally after except catches exception" $ do
    runSourceVm "try:\n  raise 1\nexcept:\n  print 2\nfinally:\n  print 3\n" `shouldBe` Right ["2", "3"]

  it "passes exception class and message to context manager __exit__" $ do
    runSourceVm "class CM:\n  def __enter__(self):\n    return self\n  def __exit__(self, t, v, tb):\n    print t\n    print v\n    return 1\nwith CM():\n  raise ValueError(\"boom\")\n" `shouldBe` Right ["ValueError", "boom"]

  it "replaces exception when context manager __exit__ raises" $ do
    runSourceVm "class CM:\n  def __enter__(self):\n    return self\n  def __exit__(self, t, v, tb):\n    raise TypeError(\"wrapped\")\nwith CM():\n  raise ValueError(\"boom\")\n" `shouldBe` Left "Type error: wrapped at 5:5"

  it "evaluates power operator with integers" $ do
    runSourceVm "print 2 ** 3\n" `shouldBe` Right ["8"]

  it "evaluates right-associative power operator" $ do
    runSourceVm "print 2 ** 3 ** 2\n" `shouldBe` Right ["512"]

  it "evaluates power operator with negative exponent" $ do
    runSourceVm "print 2 ** -1\n" `shouldBe` Right ["0.5"]

  it "propagates error raised in except suite" $ do
    runSourceVm "try:\n  raise 1\nexcept:\n  raise 2\n" `shouldBe` Left "Runtime error: 2 at 4:3"

  it "catches error in nested try when inner handler does not match" $ do
    runSourceVm "try:\n  try:\n    raise 1\n  except NameError:\n    print 1\nexcept:\n  print 2\n" `shouldBe` Right ["2"]

  it "uses inner handler when nested try handler matches" $ do
    runSourceVm "try:\n  try:\n    raise 1\n  except RuntimeError:\n    print 1\nexcept:\n  print 2\n" `shouldBe` Right ["1"]
