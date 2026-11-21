{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.VM.RunSourceVmPart4Spec (spec) where

import Data.Either (isLeft)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 4" $ do
  it "mirrors RunnerEdge: for loop over range/list/dict keys" $ do
    runSourceVm "for i in range(3):\nprint i\n" `shouldBe` Right ["0", "1", "2"]
    runSourceVm "for i in [10, 20]:\nprint i\n" `shouldBe` Right ["10", "20"]
    runSourceVm "for k in {3: 30, 1: 10}:\nprint k\n" `shouldBe` Right ["3", "1"]

  it "mirrors RunnerEdge exact source variants" $ do
    runSourceVm "def add(a, b = 2):\n  return a + b\nprint add(3)\nprint add(3, 4)\n" `shouldBe` Right ["5", "7"]
    runSourceVm "def add(a, b):\n  return a + b\nprint add(1, b=2)\n" `shouldBe` Right ["3"]
    runSourceVm "def bad(a = 1, b):\n  return a + b\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 16})"
    runSourceVm "def dup(a, a):\n  return a\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 12})"
    runSourceVm "def dup(a = 1, a = 2):\n  return a\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 16})"
    runSourceVm "def f(a, b = 2, c = d):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier d at 1:21"
    runSourceVm "def f(a, b = c, c = 2):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier c at 1:14"
    runSourceVm "def f(a):\n  return a\nprint f(a=1)\n" `shouldBe` Right ["1"]
    runSourceVm "def f(a, b):\n  return a + b\nprint f(a=len(1), b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"
    runSourceVm "def f(a, b):\n  return a + b\nprint f(1, b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:14"
    runSourceVm "def f(a, b = 1 / 0):\n  return a + b\nprint f(a=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"
    runSourceVm "for i in \"abc\":\nprint i\n" `shouldBe` Left "Type error: for expects iterable (int range, list, or dict) at 1:10"
    runSourceVm "for i in range(2000):\npass\nprint 1\n" `shouldBe` Right ["1"]
    runSourceVm "for i in range(2001):\npass\n" `shouldBe` Left "Value error: iteration limit exceeded at 1:1"
    runSourceVm "print [1, 2]\n" `shouldBe` Right ["[1, 2]"]
    runSourceVm "print {1: 2, 3: 4}\n" `shouldBe` Right ["{1: 2, 3: 4}"]
    runSourceVm "print \"a\" == \"a\"\nprint \"a\" != \"b\"\n" `shouldBe` Right ["1", "1"]
    runSourceVm "print keys({3: 30, 1: 10})\nprint values({3: 30, 1: 10})\nprint items({3: 30, 1: 10})\n" `shouldBe` Right ["[3, 1]", "[30, 10]", "[[3, 30], [1, 10]]"]
    runSourceVm "x = 0\nwhile x < 2000:\nx = x + 1\nprint x\n" `shouldBe` Right ["2000"]
    runSourceVm "x = 0\nwhile x < 2001:\nx = x + 1\nprint x\n" `shouldBe` Left "Value error: iteration limit exceeded at 2:1"

  it "returns lexer error for unexpected character" $ do
    runSourceVm "x @ 1\n" `shouldBe` Left "ExpectedAssignAfterIdentifier (Position {line = 1, column = 1})"

  it "returns lexer error for inconsistent dedent indentation" $ do
    runSourceVm "if 1:\n  print 1\n print 2\n" `shouldBe` Left "UnexpectedCharacter ' '"

  it "runs tab-indented input as leading whitespace" $ do
    runSourceVm "\tprint 1\n" `shouldBe` Right ["1"]

  it "runs assignment with tabs between tokens" $ do
    runSourceVm "x\t=\t1\nprint x\n" `shouldBe` Right ["1"]

  it "runs print statement with tab separator" $ do
    runSourceVm "x = 2\nprint\tx\n" `shouldBe` Right ["2"]

  it "reports duplicate keyword argument" $ do
    runSourceVm "def f(a):\n  return a\nprint f(a=1, a=2)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "reports unexpected keyword argument" $ do
    runSourceVm "def f(a):\n  return a\nprint f(b=2)\n" `shouldBe` Left "Argument error: unexpected keyword argument b at 3:9"

  it "reports multiple values for parameter" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, a=2)\n" `shouldBe` Left "Argument error: multiple values for parameter a at 3:12"

  it "rejects keyword arguments for builtin call" $ do
    runSourceVm "print len(x=[1])\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 1:11"

  it "prioritizes builtin keyword rejection over keyword expression error" $ do
    runSourceVm "print len(x=len(1))\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 1:11"

  it "prioritizes first keyword evaluation error over duplicate keyword detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(a=len(1), a=2)\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes duplicate keyword detection over second keyword evaluation error" $ do
    runSourceVm "def f(a):\n  return a\nprint f(a=1, a=len(1))\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "reports first duplicate keyword argument in source order" $ do
    runSourceVm "def f(a, b):\n  return a + b\nprint f(a=1, b=2, a=3, b=4)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:19"

  it "prioritizes first keyword Name error over duplicate keyword detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(a=missing, a=2)\n" `shouldBe` Left "Name error: undefined identifier missing at 3:11"

  it "prioritizes duplicate keyword detection over second keyword Name error" $ do
    runSourceVm "def f(a):\n  return a\nprint f(a=1, a=missing)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "reports first multiple-values collision in source order" $ do
    runSourceVm "def f(a, b):\n  return a + b\nprint f(1, 2, b=3, a=4)\n" `shouldBe` Left "Argument error: multiple values for parameter b at 3:15"

  it "prioritizes keyword evaluation error over multiple values detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, a=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:14"

  it "prioritizes keyword Name error over multiple values detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, a=missing)\n" `shouldBe` Left "Name error: undefined identifier missing at 3:14"

  it "prioritizes nested builtin keyword rejection over multiple values detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, a=len(x=[1]))\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 3:18"

  it "prioritizes nested Name error over multiple values detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, a=len(missing))\n" `shouldBe` Left "Name error: undefined identifier missing at 3:18"

  it "reports first unexpected keyword argument in source order" $ do
    runSourceVm "def f(a):\n  return a\nprint f(z=1, b=2)\n" `shouldBe` Left "Argument error: unexpected keyword argument z at 3:9"

  it "prioritizes keyword evaluation error over unexpected keyword detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes keyword Name error over unexpected keyword detection" $ do
    runSourceVm "def f(a):\n  return a\nprint f(b=missing)\n" `shouldBe` Left "Name error: undefined identifier missing at 3:11"

  it "prioritizes builtin keyword rejection over keyword argument Name error" $ do
    runSourceVm "print len(x=missing)\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 1:11"

  it "reports method-style builtin keyword argument as unsupported" $ do
    runSourceVm "d = {}\nprint d.update(k=1)\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 2:16"

  it "prioritizes method-style builtin keyword rejection over keyword argument expression error" $ do
    runSourceVm "d = {}\nprint d.update(k=len(1))\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 2:16"

  it "prioritizes unexpected keyword error over multiple values error" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, a=2, b=3)\n" `shouldBe` Left "Argument error: unexpected keyword argument b at 3:17"

  it "prioritizes duplicate keyword error over unexpected keyword error" $ do
    runSourceVm "def f(a):\n  return a\nprint f(b=1, b=2)\n" `shouldBe` Left "Argument error: duplicate keyword argument b at 3:14"

  it "prioritizes unexpected keyword error over count mismatch" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, b=2)\n" `shouldBe` Left "Argument error: unexpected keyword argument b at 3:12"

  it "reports first unexpected keyword position over count mismatch" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, z=2, b=3)\n" `shouldBe` Left "Argument error: unexpected keyword argument z at 3:12"

  it "prioritizes multiple values error over count mismatch" $ do
    runSourceVm "def f(a):\n  return a\nprint f(1, 2, a=3)\n" `shouldBe` Left "Argument error: multiple values for parameter a at 3:15"

  it "returns parse error for mixed keyword then positional call arguments" $ do
    runSourceVm "print f(a=1, 2)\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 14})"

  it "supports star expansion in call arguments" $ do
    runSourceVm "def f(a, b):\n  return a + b\nprint f(*[1,2])\n" `shouldBe` Right ["3"]

  it "supports double-star expansion in call arguments" $ do
    runSourceVm "def f(a, b):\n  return a + b\nprint f(**{\"a\":1,\"b\":2})\n" `shouldBe` Right ["3"]

  it "supports *args in function definition" $ do
    runSourceVm "def f(*args):\n  return len(args)\nprint f(1,2,3)\n" `shouldBe` Right ["3"]

  it "supports **kwargs in function definition" $ do
    runSourceVm "def f(**kwargs):\n  return get(kwargs, \"x\", 0)\nprint f(x=7)\n" `shouldBe` Right ["7"]

  it "returns parse error for unsupported keyword-only separator in function definition" $ do
    runSourceVm "def f(*, a):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported positional-only separator in function definition" $ do
    runSourceVm "def f(a, /):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 10})"

  it "accepts type annotation in function parameter" $ do
    runSourceVm "def f(a: int):\n  return a\nprint f(1)\n" `shouldBe` Right ["1"]

  it "accepts return type annotation in function definition" $ do
    runSourceVm "def f() -> int:\n  return 1\nprint f()\n" `shouldBe` Right ["1"]

  it "returns parse error for missing expression after print" $ do
    runSourceVm "print\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 6})"

  it "returns parse error for malformed global statement" $ do
    runSourceVm "global\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for if header without suite body" $ do
    runSourceVm "if 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for def header without suite body" $ do
    runSourceVm "def f():\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for while header without suite body" $ do
    runSourceVm "while 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"
