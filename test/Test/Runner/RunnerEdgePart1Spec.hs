{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Runner.RunnerEdgePart1Spec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration edge/error) - part 1" $ do
  it "prints unary minus integer literal" $ do
    runSource "print -2\n" `shouldBe` Right ["-2"]

  it "evaluates unary minus over identifier and parenthesized expression" $ do
    runSource "x = 3\nprint -x\nprint -(1 + 2)\n" `shouldBe` Right ["-3", "-3"]

  it "reports unary minus type error" $ do
    runSource "print -\"x\"\n" `shouldBe` Left "Type error: unary - expects int at 1:7"

  it "evaluates values and items builtins for dictionary" $ do
    runSource "print values({1: 2, 3: 4})\nprint items({1: 2, 3: 4})\n" `shouldBe` Right ["[2, 4]", "[[1, 2], [3, 4]]"]

  it "preserves insertion order for dictionary builtins" $ do
    runSource "print keys({3: 30, 1: 10})\nprint values({3: 30, 1: 10})\nprint items({3: 30, 1: 10})\n" `shouldBe` Right ["[3, 1]", "[30, 10]", "[[3, 30], [1, 10]]"]

  it "supports method-call style for builtins" $ do
    runSource "x = [1, 2]\nprint x.append(3)\n" `shouldBe` Right ["[1, 2, 3]"]

  it "reports method-call style builtin type error with method position" $ do
    runSource "print 1.append(2)\n" `shouldBe` Left "Type error: append expects list as first argument at 1:9"

  it "reports dictionary method-call style errors with method position" $ do
    runSource "print 1.get(1)\n" `shouldBe` Left "Type error: get expects dict as first argument at 1:9"
    runSource "print {}.setdefault(1)\n" `shouldBe` Right ["{1: None}"]
    runSource "print 1.setdefault(1)\n" `shouldBe` Left "Type error: setdefault expects dict as first argument at 1:9"
    runSource "print {}.update(1)\n" `shouldBe` Left "Type error: update expects dict as second argument at 1:10"

  it "reports builtin count mismatch at call-site position" $ do
    runSource "print len()\n" `shouldBe` Left "Argument count mismatch when calling len at 1:7"
    runSource "print len(1, 2)\n" `shouldBe` Left "Argument count mismatch when calling len at 1:7"
    runSource "print {}.update()\n" `shouldBe` Left "Argument count mismatch when calling update at 1:10"
    runSource "print {}.setdefault()\n" `shouldBe` Left "Argument count mismatch when calling setdefault at 1:10"

  it "evaluates string equality and inequality" $ do
    runSource "print \"a\" == \"a\"\nprint \"a\" != \"b\"\n" `shouldBe` Right ["1", "1"]

  it "prints list literal" $ do
    runSource "print [1, 2]\n" `shouldBe` Right ["[1, 2]"]

  it "prints dictionary literal" $ do
    runSource "print {1: 2, 3: 4}\n" `shouldBe` Right ["{1: 2, 3: 4}"]

  it "evaluates for loop with range builtin" $ do
    runSource "for i in range(3):\nprint i\n" `shouldBe` Right ["0", "1", "2"]

  it "evaluates for loop over list literal" $ do
    runSource "for i in [10, 20]:\nprint i\n" `shouldBe` Right ["10", "20"]

  it "evaluates for loop over dictionary keys" $ do
    runSource "for k in {3: 30, 1: 10}:\nprint k\n" `shouldBe` Right ["3", "1"]

  it "reports break/continue outside loop" $ do
    runSource "break\n" `shouldBe` Left "Break outside loop at 1:1"
    runSource "continue\n" `shouldBe` Left "Continue outside loop at 1:1"

  it "reports for-loop iterable type error" $ do
    runSource "for i in \"abc\":\nprint i\n" `shouldBe` Left "Type error: for expects iterable (int range, list, or dict) at 1:10"

  it "enforces iteration limit boundary for while and for loops" $ do
    runSource "x = 0\nwhile x < 2000:\nx = x + 1\nprint x\n" `shouldBe` Right ["2000"]
    runSource "x = 0\nwhile x < 2001:\nx = x + 1\nprint x\n" `shouldBe` Left "Value error: iteration limit exceeded at 2:1"
    runSource "for i in range(2000):\npass\nprint 1\n" `shouldBe` Right ["1"]
    runSource "for i in range(2001):\npass\n" `shouldBe` Left "Value error: iteration limit exceeded at 1:1"

  it "returns lexer error for unexpected character" $ do
    runSource "x @ 1\n" `shouldBe` Left "ExpectedAssignAfterIdentifier (Position {line = 1, column = 1})"

  it "returns lexer error for inconsistent dedent indentation" $ do
    runSource "if 1:\n  print 1\n print 2\n" `shouldBe` Left "UnexpectedCharacter ' '"

  it "runs tab-indented input as leading whitespace" $ do
    runSource "\tprint 1\n" `shouldBe` Right ["1"]

  it "runs assignment with tabs between tokens" $ do
    runSource "x\t=\t1\nprint x\n" `shouldBe` Right ["1"]

  it "runs print statement with tab separator" $ do
    runSource "x = 2\nprint\tx\n" `shouldBe` Right ["2"]

  it "returns parse error for missing expression after print" $ do
    runSource "print\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 6})"

  it "returns parse error for malformed global statement" $ do
    runSource "global\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for if header without suite body" $ do
    runSource "if 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for def header without suite body" $ do
    runSource "def f():\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for while header without suite body" $ do
    runSource "while 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for for header without suite body" $ do
    runSource "for i in 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for standalone elif header" $ do
    runSource "elif 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for standalone else header" $ do
    runSource "else:\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for unexpected leading indentation" $ do
    runSource "  print 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for if header followed by blank line only" $ do
    runSource "if 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 3, column = 1})"

  it "returns parse error for while header followed by blank line only" $ do
    runSource "while 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 3, column = 1})"

  it "returns parse error for if header missing colon" $ do
    runSource "if 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 5})"

  it "returns parse error for while header missing colon" $ do
    runSource "while 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 8})"

  it "returns parse error for for header missing colon" $ do
    runSource "for i in 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 11})"

  it "returns parse error for def header missing colon" $ do
    runSource "def f()\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 8})"

  it "executes function and collects prints from body and call" $ do
    runSource "def add(a, b):\nprint a\nprint add(1, 2)\n" `shouldBe` Right ["1", "0"]

  it "supports function default arguments for omitted trailing parameters" $ do
    runSource "def add(a, b = 2):\n  return a + b\nprint add(3)\nprint add(3, 4)\n" `shouldBe` Right ["5", "7"]

  it "supports function default arguments with trailing comma in parameter list" $ do
    runSource "def add(a, b = 2,):\n  return a + b\nprint add(3)\n" `shouldBe` Right ["5"]

  it "reports argument count mismatch when required parameter is missing even with defaults" $ do
    runSource "def add(a, b = 2):\n  return a + b\nprint add()\n" `shouldBe` Left "Argument count mismatch when calling add at 3:7"

  it "reports count mismatch at call-site position for too many positional args" $ do
    runSource "def add(a):\n  return a\nprint add(1, 2)\n" `shouldBe` Left "Argument count mismatch when calling add at 3:7"

  it "reports count mismatch at call-site position for missing required keyword set" $ do
    runSource "def add(a, b):\n  return a + b\nprint add(a=1)\n" `shouldBe` Left "Argument count mismatch when calling add at 3:7"

  it "returns parse error when required parameter appears after default parameter" $ do
    runSource "def bad(a = 1, b):\n  return a + b\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 16})"

  it "returns parse error when function parameters contain duplicate names" $ do
    runSource "def dup(a, a):\n  return a\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 12})"

  it "returns parse error when duplicate parameter names include defaults" $ do
    runSource "def dup(a = 1, a = 2):\n  return a\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 16})"

  it "supports keyword argument call for user-defined function" $ do
    runSource "def f(a):\n  return a\nprint f(a=1)\n" `shouldBe` Right ["1"]

  it "supports mixed positional then keyword call arguments" $ do
    runSource "def add(a, b):\n  return a + b\nprint add(1, b=2)\n" `shouldBe` Right ["3"]

  it "reports duplicate keyword argument with argument name and position" $ do
    runSource "def f(a):\n  return a\nprint f(a=1, a=2)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "reports first duplicate keyword argument in source order" $ do
    runSource "def f(a, b):\n  return a + b\nprint f(a=1, b=2, a=3, b=4)\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:19"

  it "prioritizes first keyword evaluation error over duplicate keyword detection" $ do
    runSource "def f(a):\n  return a\nprint f(a=len(1), a=2)\n" `shouldBe` Left "Type error: len expects string or list at 3:11"

  it "prioritizes duplicate keyword detection over second keyword evaluation error" $ do
    runSource "def f(a):\n  return a\nprint f(a=1, a=len(1))\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

  it "prioritizes first keyword Name error over duplicate keyword detection" $ do
    runSource "def f(a):\n  return a\nprint f(a=missing, a=2)\n" `shouldBe` Left "Name error: undefined identifier missing at 3:11"
