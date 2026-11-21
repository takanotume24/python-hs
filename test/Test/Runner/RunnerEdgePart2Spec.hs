{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Runner.RunnerEdgePart2Spec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration edge/error) - part 2" $ do
  it "prioritizes duplicate keyword detection over second keyword Name error" $ do
    runSource "def f(a):\n  return a\nprint f(a=1, a=missing)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "reports multiple values for parameter when positional and keyword both bind it" $ do
    runSource "def f(a):\n  return a\nprint f(1, a=2)\n" `shouldBe` Left "Argument error: multiple values for parameter a at 3:12"

  it "reports first multiple-values collision in source order" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(1, 2, b=3, a=4)\n" `shouldBe` Left "Argument error: multiple values for parameter b at 3:15"

  it "prioritizes keyword evaluation error over multiple values detection" $ do
    runSource "def f(a):\n  return a\nprint f(1, a=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:14"

  it "prioritizes keyword Name error over multiple values detection" $ do
    runSource "def f(a):\n  return a\nprint f(1, a=missing)\n" `shouldBe` Left "Name error: undefined identifier missing at 3:14"

  it "prioritizes nested builtin keyword rejection over multiple values detection" $ do
    runSource "def f(a):\n  return a\nprint f(1, a=len(x=[1]))\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 3:18"

  it "prioritizes nested Name error over multiple values detection" $ do
    runSource "def f(a):\n  return a\nprint f(1, a=len(missing))\n" `shouldBe` Left "Name error: undefined identifier missing at 3:18"

  it "reports unexpected keyword argument with argument name and position" $ do
    runSource "def f(a):\n  return a\nprint f(b=2)\n" `shouldBe` Left "Argument error: unexpected keyword argument b at 3:9"

  it "reports first unexpected keyword argument in source order" $ do
    runSource "def f(a):\n  return a\nprint f(z=1, b=2)\n" `shouldBe` Left "Argument error: unexpected keyword argument z at 3:9"

  it "prioritizes keyword evaluation error over unexpected keyword detection" $ do
    runSource "def f(a):\n  return a\nprint f(b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes keyword Name error over unexpected keyword detection" $ do
    runSource "def f(a):\n  return a\nprint f(b=missing)\n" `shouldBe` Left "Name error: undefined identifier missing at 3:11"

  it "reports builtin keyword argument as unsupported" $ do
    runSource "print len(x=[1])\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 1:11"

  it "prioritizes builtin keyword rejection over keyword argument expression error" $ do
    runSource "print len(x=len(1))\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 1:11"

  it "prioritizes builtin keyword rejection over keyword argument Name error" $ do
    runSource "print len(x=missing)\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 1:11"

  it "reports method-style builtin keyword argument as unsupported" $ do
    runSource "d = {}\nprint d.update(k=1)\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 2:16"

  it "prioritizes method-style builtin keyword rejection over keyword argument expression error" $ do
    runSource "d = {}\nprint d.update(k=len(1))\n" `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 2:16"

  it "prioritizes duplicate keyword detection over default builtin keyword rejection" $ do
    runSource "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1, a=2, b=3)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "prioritizes unexpected keyword detection over default builtin keyword rejection" $ do
    runSource "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1, b=2, d=3)\n" `shouldBe` Left "Argument error: unexpected keyword argument d at 3:19"

  it "prioritizes multiple values detection over default builtin keyword rejection" $ do
    runSource "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(1, a=2, b=3)\n" `shouldBe` Left "Argument error: multiple values for parameter a at 3:12"

  it "reports runtime error position from default expression evaluation" $ do
    runSource "def f(a, b = 1 / 0):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Value error: division by zero at 1:16"

  it "reports Name error position from default expression evaluation" $ do
    runSource "def f(a, b = missing):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier missing at 1:14"

  it "reports builtin type error position from default expression evaluation" $ do
    runSource "def f(a, b = len(1)):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Type error: len expects string or list at 1:14"

  it "reports Name error when default expression references unknown identifier" $ do
    runSource "def f(a, b = 2, c = d):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier d at 1:21"

  it "reports Name error when default expression references later parameter" $ do
    runSource "def f(a, b = c, c = 2):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier c at 1:14"

  it "reports Name error position from composite default expression" $ do
    runSource "def f(a, b = 2, c = b + d):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier d at 1:25"

  it "reports builtin type error from default expression using bound parameter" $ do
    runSource "def f(a, b = len(a)):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Type error: len expects string or list at 1:14"

  it "prioritizes explicit argument evaluation error over default expression error" $ do
    runSource "def f(a, b = 1 / 0):\n  return a + b\nprint f(a=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes positional argument evaluation error over default expression error" $ do
    runSource "def f(a, b = 1 / 0):\n  return a + b\nprint f(len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:9"

  it "prioritizes explicit argument evaluation error over default builtin keyword rejection" $ do
    runSource "def f(a, b = len(x=[1])):\n  return a + b\nprint f(a=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes positional argument evaluation error over default builtin keyword rejection" $ do
    runSource "def f(a, b = len(x=[1])):\n  return a + b\nprint f(len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:9"

  it "prioritizes count mismatch over default builtin keyword rejection with keyword call" $ do
    runSource "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1)\n" `shouldBe` Left "Argument count mismatch when calling f at 3:7"

  it "prioritizes count mismatch over default builtin keyword rejection with positional call" $ do
    runSource "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(1)\n" `shouldBe` Left "Argument count mismatch when calling f at 3:7"

  it "prioritizes leftmost argument evaluation error in mixed positional and keyword call" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(len(1), b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:9"

  it "prioritizes leftmost argument evaluation error in keyword-only call" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(a=len(1), b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "reports right keyword argument evaluation error after left keyword succeeds" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(a=1, b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:16"

  it "reports keyword argument evaluation error after positional argument succeeds" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(1, b=len(1))\n" `shouldBe` Left "Type error: len expects string or list at 3:14"

  it "reports Name error position inside builtin default list expression" $ do
    runSource "def f(a, b = len([a, missing])):\n  return a + b\nprint f(1)\n" `shouldBe` Left "Name error: undefined identifier missing at 1:22"

  it "prioritizes duplicate keyword error over unexpected keyword error" $ do
    runSource "def f(a):\n  return a\nprint f(b=1, b=2)\n" `shouldBe` Left "Argument error: duplicate keyword argument b at 3:14"

  it "prioritizes unexpected keyword error over count mismatch" $ do
    runSource "def f(a):\n  return a\nprint f(1, b=2)\n" `shouldBe` Left "Argument error: unexpected keyword argument b at 3:12"

  it "reports first unexpected keyword position over count mismatch" $ do
    runSource "def f(a):\n  return a\nprint f(1, z=2, b=3)\n" `shouldBe` Left "Argument error: unexpected keyword argument z at 3:12"

  it "prioritizes multiple values error over count mismatch" $ do
    runSource "def f(a):\n  return a\nprint f(1, 2, a=3)\n" `shouldBe` Left "Argument error: multiple values for parameter a at 3:15"

  it "prioritizes unexpected keyword error over multiple values error" $ do
    runSource "def f(a):\n  return a\nprint f(1, a=2, b=3)\n" `shouldBe` Left "Argument error: unexpected keyword argument b at 3:17"

  it "returns parse error for mixed keyword then positional call arguments" $ do
    runSource "print f(a=1, 2)\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 14})"

  it "supports star expansion in call arguments" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(*[1,2])\n" `shouldBe` Left "Runtime error: argument expansion is only supported in vm engine at 3:9"

  it "supports double-star expansion in call arguments" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(**{\"a\":1,\"b\":2})\n" `shouldBe` Left "Runtime error: argument expansion is only supported in vm engine at 3:9"

  it "supports *args in function definition" $ do
    runSource "def f(*args):\n  return len(args)\nprint f(1,2,3)\n" `shouldBe` Left "Argument count mismatch when calling f at 3:7"

  it "supports **kwargs in function definition" $ do
    runSource "def f(**kwargs):\n  return get(kwargs, \"x\", 0)\nprint f(x=7)\n" `shouldBe` Left "Argument error: unexpected keyword argument x at 3:9"

  it "returns parse error for unsupported keyword-only separator in function definition" $ do
    runSource "def f(*, a):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported positional-only separator in function definition" $ do
    runSource "def f(a, /):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 10})"

  it "accepts type annotation in function parameter" $ do
    runSource "def f(a: int):\n  return a\nprint f(1)\n" `shouldBe` Right ["1"]

  it "accepts return type annotation in function definition" $ do
    runSource "def f() -> int:\n  return 1\nprint f()\n" `shouldBe` Right ["1"]

  it "reports update merge type error for non-dict second argument" $ do
    runSource "print update({}, 1)\n" `shouldBe` Left "Type error: update expects dict as second argument at 1:7"

  it "allows function to read global variable" $ do
    runSource "x = 7\ndef readGlobal():\nreturn x\nprint readGlobal()\n" `shouldBe` Right ["7"]

  it "prefers function parameter over global variable" $ do
    runSource "x = 7\ndef echo(x):\nreturn x\nprint echo(99)\n" `shouldBe` Right ["99"]

  it "updates global variable when declared with global inside function" $ do
    runSource "x = 10\ndef setGlobal():\n  global x\n  x = 99\n  return x\nprint setGlobal()\nprint x\n" `shouldBe` Right ["99", "99"]

  it "creates a new global variable when declared in function" $ do
    runSource "def makeGlobal():\n  global y\n  y = 5\nprint makeGlobal()\nprint y\n" `shouldBe` Right ["0", "5"]

  it "treats global declaration in conditional branch as function-wide" $ do
    runSource "x = 1\ndef setViaBranch():\n  if 0:\n    global x\n  x = 2\nprint setViaBranch()\nprint x\n" `shouldBe` Right ["0", "2"]
