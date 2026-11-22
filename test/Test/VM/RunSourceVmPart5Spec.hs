{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.VM.RunSourceVmPart5Spec (spec) where

import Data.Either (isLeft)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 5" $ do
  it "returns parse error for for header without suite body" $ do
    runSourceVm "for i in 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for standalone elif header" $ do
    runSourceVm "elif 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for standalone else header" $ do
    runSourceVm "else:\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for unexpected leading indentation" $ do
    runSourceVm "  print 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for if header followed by blank line only" $ do
    runSourceVm "if 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 3, column = 1})"

  it "returns parse error for while header followed by blank line only" $ do
    runSourceVm "while 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 3, column = 1})"

  it "returns parse error for if header missing colon" $ do
    runSourceVm "if 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 5})"

  it "returns parse error for while header missing colon" $ do
    runSourceVm "while 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 8})"

  it "returns parse error for for header missing colon" $ do
    runSourceVm "for i in 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 11})"

  it "returns parse error for def header missing colon" $ do
    runSourceVm "def f()\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 8})"

  it "prioritizes duplicate keyword detection over default builtin keyword rejection" $ do
    runSourceVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1, a=2, b=3)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "prioritizes unexpected keyword detection over default builtin keyword rejection" $ do
    runSourceVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1, b=2, d=3)\n" `shouldBe` Left "Argument error: unexpected keyword argument d at 3:19"

  it "reports runtime error position from default expression evaluation" $ do
    runSourceVm "def f(a, b = 1 / 0):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Value error: division by zero at 1:16"

  it "prioritizes explicit argument evaluation error over default builtin keyword rejection" $ do
    runSourceVm "def f(a, b = len(x=[1])):\n  return a + b\nprint f(a=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes count mismatch over default builtin keyword rejection with positional call" $ do
    runSourceVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Argument count mismatch when calling f at 3:7"

  it "prioritizes multiple values detection over default builtin keyword rejection" $ do
    runSourceVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(1, a=2, b=3)\n" `shouldBe` Left "Argument error: multiple values for parameter a at 3:12"

  it "reports Name error position from default expression evaluation" $ do
    runSourceVm "def f(a, b = missing):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier missing at 1:14"

  it "reports builtin type error position from default expression evaluation" $ do
    runSourceVm "def f(a, b = len(1)):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Type error: len expects string or list at 1:14"

  it "reports Name error position from composite default expression" $ do
    runSourceVm "def f(a, b = 2, c = b + d):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier d at 1:25"

  it "reports builtin type error from default expression using bound parameter" $ do
    runSourceVm "def f(a, b = len(a)):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Type error: len expects string or list at 1:14"

  it "prioritizes positional argument evaluation error over default expression error" $ do
    runSourceVm "def f(a, b = 1 / 0):\n  return a + b\nprint f(len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:9"

  it "prioritizes positional argument evaluation error over default builtin keyword rejection" $ do
    runSourceVm "def f(a, b = len(x=[1])):\n  return a + b\nprint f(len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:9"

  it "prioritizes count mismatch over default builtin keyword rejection with keyword call" $ do
    runSourceVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1)\n" `shouldBe` Left "Argument count mismatch when calling f at 3:7"

  it "prioritizes leftmost argument evaluation error in mixed positional and keyword call" $ do
    runSourceVm "def f(a, b):\n  return a + b\nprint f(len(1), b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:9"

  it "reports right keyword argument evaluation error after left keyword succeeds" $ do
    runSourceVm "def f(a, b):\n  return a + b\nprint f(a=1, b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:16"

  it "reports Name error position inside builtin default list expression" $ do
    runSourceVm "def f(a, b = len([a, missing])):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier missing at 1:22"

  it "evaluates float and multiplicative operators" $ do
    runSourceVm "print 1.5 + 2\nprint 7 / 2\nprint 7 // 2\nprint 7 % 4\n" `shouldBe` Right ["3.5", "3.5", "3", "3"]

  it "evaluates numeric comparisons" $ do
    runSourceVm "print 1 < 2\nprint 2 == 2.0\nprint 3 >= 4\n" `shouldBe` Right ["1", "1", "0"]

  it "evaluates and/or with short-circuit boolean semantics" $ do
    runSourceVm "print 0 and 1\nprint 1 and 2\nprint 1 or 0\nprint 0 or 0\n" `shouldBe` Right ["0", "1", "1", "0"]

  it "does not evaluate right-hand side when and/or short-circuits" $ do
    runSourceVm "def probe(x):\n  print x\n  return x\nprint 0 and probe(1)\nprint 1 or probe(2)\n" `shouldBe` Right ["0", "1"]

  it "evaluates unary minus and not expressions" $ do
    runSourceVm "x = 2\nprint -x\nprint not 0\nprint not 1\nprint not None\n" `shouldBe` Right ["-2", "1", "0", "1"]

  it "evaluates for loop over int, list, and dict keys" $ do
    runSourceVm "for i in 3:\n  print i\nfor x in [10, 20]:\n  print x\nfor k in {3: 30, 1: 10}:\n  print k\n" `shouldBe` Right ["0", "1", "2", "10", "20", "3", "1"]

  it "reports for-loop iterable type error" $ do
    runSourceVm "for i in \"abc\":\n  print i\n" `shouldBe` Left "Type error: for expects iterable (int range, list, or dict) at 1:10"

  it "supports break and continue in while loop" $ do
    runSourceVm "x = 0\nwhile x < 5:\n  x = x + 1\n  if x == 2:\n    continue\n  if x == 4:\n    break\n  print x\n" `shouldBe` Right ["1", "3"]

  it "reports break and continue outside loop" $ do
    runSourceVm "break\n" `shouldBe` Left "Break outside loop at 1:1"
    runSourceVm "continue\n" `shouldBe` Left "Continue outside loop at 1:1"

  it "enforces iteration limit boundary for while and for loops" $ do
    runSourceVm "x = 0\nwhile x < 2000:\n  x = x + 1\nprint x\n" `shouldBe` Right ["2000"]
    runSourceVm "x = 0\nwhile x < 2001:\n  x = x + 1\nprint x\n" `shouldBe` Left "Value error: iteration limit exceeded at 2:1"
    runSourceVm "for i in 2000:\n  pass\nprint 1\n" `shouldBe` Right ["1"]
    runSourceVm "for i in 2001:\n  pass\n" `shouldBe` Left "Value error: iteration limit exceeded at 1:1"

  it "evaluates compound assignment operators" $ do
    runSourceVm "x = 1\nx += 2\nx -= 1\nx *= 3\nx /= 2\nx %= 2\nx //= 1\nprint x\n" `shouldBe` Right ["1.0"]

  it "updates global variable when declared with global inside function" $ do
    runSourceVm "x = 10\ndef setGlobal():\n  global x\n  x = 99\n  return x\nprint setGlobal()\nprint x\n" `shouldBe` Right ["99", "99"]

  it "evaluates len and bool builtins" $ do
    runSourceVm "print len([1, 2, 3])\nprint bool(0)\nprint bool([1])\n" `shouldBe` Right ["3", "0", "1"]

  it "evaluates range builtin with one two and three args" $ do
    runSourceVm "print range(3)\nprint range(1, 5)\nprint range(5, 1, -2)\n" `shouldBe` Right ["[0, 1, 2]", "[1, 2, 3, 4]", "[5, 3]"]

  it "reports builtin argument and type errors" $ do
    runSourceVm "print len(1)\n" `shouldBe` Left "Type error: len expects string or list at 1:7"
    runSourceVm "print bool()\n" `shouldBe` Right ["0"]
    runSourceVm "print range(1, 2, 0)\n" `shouldBe` Left "Value error: range step must not be zero at 1:7"

  it "evaluates append sort and reverse builtins" $ do
    runSourceVm "print append([1, 2], 3)\nprint sort([3, 1.5, 2])\nprint reverse([1, 2, 3])\n" `shouldBe` Right ["[1, 2, 3]", "[1.5, 2, 3]", "[3, 2, 1]"]

  it "reports append sort and reverse builtin errors" $ do
    runSourceVm "print append(1, 2)\n" `shouldBe` Left "Type error: append expects list as first argument at 1:7"
    runSourceVm "print append([1])\n" `shouldBe` Left "Argument count mismatch when calling append at 1:7"
    runSourceVm "print sort([1, \"x\"])\n" `shouldBe` Left "Type error: sort expects list of number or string at 1:7"
    runSourceVm "print reverse(1)\n" `shouldBe` Left "Type error: reverse expects list as first argument at 1:7"

  it "evaluates remove insert and pop builtins" $ do
    runSourceVm "print remove([1, 2, 1], 1)\nprint insert([1, 3], 1, 2)\nprint pop([1, 2, 3])\nprint pop({1: 10}, 1)\nprint pop({}, 1, 99)\n" `shouldBe` Right ["[2, 1]", "[1, 2, 3]", "3", "10", "99"]

  it "reports remove insert and pop builtin errors" $ do
    runSourceVm "print remove([1], 2)\n" `shouldBe` Left "Value error: remove value not found at 1:7"
    runSourceVm "print remove(1, 2)\n" `shouldBe` Left "Type error: remove expects list as first argument at 1:7"
    runSourceVm "print insert([1], \"0\", 2)\n" `shouldBe` Left "Type error: insert expects int index at 1:7"
    runSourceVm "print insert([1], 0)\n" `shouldBe` Left "Argument count mismatch when calling insert at 1:7"
    runSourceVm "print pop([])\n" `shouldBe` Left "Value error: pop from empty list at 1:7"
    runSourceVm "print pop({}, 1)\n" `shouldBe` Left "Key not found in pop at 1:7"
    runSourceVm "print pop(1)\n" `shouldBe` Left "Type error: pop expects list at 1:7"

  it "evaluates dict-family builtins" $ do
    runSourceVm "print clear([1, 2])\nprint clear({1: 10})\nprint keys({2: 20, 1: 10})\nprint get({1: 10}, 1)\nprint get({}, 1, 99)\nprint update({1: 10}, 2, 20)\nprint update({1: 10}, {2: 20})\nprint setdefault({1: 10}, 2)\nprint setdefault({1: 10}, 1, 99)\nprint values({2: 20, 1: 10})\nprint items({2: 20, 1: 10})\n" `shouldBe` Right ["[]", "{}", "[2, 1]", "10", "99", "{1: 10, 2: 20}", "{1: 10, 2: 20}", "{1: 10, 2: None}", "{1: 10}", "[20, 10]", "[[2, 20], [1, 10]]"]
