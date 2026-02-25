module Test.Runner.RunnerEdgeSpec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration edge/error)" $ do
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

  it "reports iteration limit exceeded for while and for loops" $ do
    runSource "x = 0\nwhile x < 10001:\nx = x + 1\nprint x\n" `shouldBe` Left "Value error: iteration limit exceeded at 2:1"
    runSource "for i in range(10001):\npass\n" `shouldBe` Left "Value error: iteration limit exceeded at 1:1"

  it "returns lexer error for unexpected character" $ do
    runSource "x @ 1\n" `shouldBe` Left "UnexpectedCharacter '@'"

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
    runSource "if 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for while header followed by blank line only" $ do
    runSource "while 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

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

  it "runs if/else and function return in one script" $ do
    runSource "x = 0\nif x:\nprint 1\nelse:\nprint 2\ndef id(v):\nreturn v\nprint id(7)\n" `shouldBe` Right ["2", "7"]

  it "runs if/elif/else and selects elif branch" $ do
    runSource "x = 0\ny = 1\nif x:\nprint 10\nelif y:\nprint 20\nelse:\nprint 30\n" `shouldBe` Right ["20"]

  it "skips while body when condition is false" $ do
    runSource "x = 0\nwhile x < 0:\nprint x\nprint 9\n" `shouldBe` Right ["9"]

  it "executes while loop body repeatedly until condition turns false" $ do
    runSource "x = 0\nwhile x < 3:\nx = x + 1\nprint x\n" `shouldBe` Right ["3"]
