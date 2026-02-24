module Test.Runner.RunnerSpec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration)" $ do
  it "runs a simple script (assignment + print)" $ do
    runSource "x = 1\nprint x\n" `shouldBe` Right ["1"]

  it "prints a string literal" $ do
    runSource "print \"hello\"\n" `shouldBe` Right ["hello"]

  it "prints True/False/None literals" $ do
    runSource "print True\nprint False\nprint None\n" `shouldBe` Right ["1", "0", "None"]

  it "treats string/list/dict values as truthy or falsy in if/not" $ do
    runSource "if \"\":\nprint 1\nelse:\nprint 2\nif []:\nprint 3\nelse:\nprint 4\nif {}:\nprint 5\nelse:\nprint 6\nprint not \"\"\nprint not [1]\n" `shouldBe` Right ["2", "4", "6", "1", "0"]

  it "handles global statement" $ do
    runSource "global x\nx = 1\nprint x\n" `shouldBe` Right ["1"]

  it "handles global statement inside function body" $ do
    runSource "def mark():\nglobal x\nprint mark()\n" `shouldBe` Right ["0"]

  it "handles pass statement" $ do
    runSource "pass\nprint 1\n" `shouldBe` Right ["1"]

  it "handles pass statement inside function body" $ do
    runSource "def nop():\npass\nprint nop()\n" `shouldBe` Right ["0"]

  it "allows pass inside if/else" $ do
    runSource "if 0:\npass\nelse:\npass\nprint 1\n" `shouldBe` Right ["1"]
 
  it "allows pass inside while body" $ do
    runSource "while 0:\npass\nprint 1\n" `shouldBe` Right ["1"]
 
  it "allows pass inside for body" $ do
    runSource "for i in [1]:\npass\nprint 2\n" `shouldBe` Right ["2"]

  it "runs multiple statements in an indented if suite" $ do
    runSource "if 1:\n  print 1\n  print 2\n" `shouldBe` Right ["1", "2"]

  it "runs multiple statements in an indented while suite" $ do
    runSource "x = 0\nwhile x < 3:\n  pass\n  x = x + 1\nprint x\n" `shouldBe` Right ["3"]

  it "runs multiple statements in an indented for suite" $ do
    runSource "sum = 0\nfor i in [1, 2]:\n  pass\n  sum += i\nprint sum\n" `shouldBe` Right ["3"]

  it "runs indented if/elif/else suites with multiple statements" $ do
    runSource "x = 0\nif x:\n  pass\n  print 1\nelif 1:\n  pass\n  print 2\nelse:\n  pass\n  print 3\n" `shouldBe` Right ["2"]

  it "continues with statements after indented if/elif/else suites" $ do
    runSource "x = 0\nif x:\n  pass\n  print 1\nelif 1:\n  pass\n  print 2\nelse:\n  pass\n  print 3\nprint 9\n" `shouldBe` Right ["2", "9"]

  it "runs nested conditional suites inside elif branch and continues" $ do
    runSource "x = 0\ny = 1\ninner = 0\nif x:\n  print 1\nelif y:\n  if inner:\n    print 10\n  else:\n    print 20\nelse:\n  print 30\nprint 9\n" `shouldBe` Right ["20", "9"]

  it "runs nested conditional inside while suite and continues" $ do
    runSource "x = 0\nwhile x < 1:\n  if 0:\n    print 10\n  else:\n    print 20\n  x = x + 1\nprint 9\n" `shouldBe` Right ["20", "9"]

  it "runs nested conditional inside for suite and continues" $ do
    runSource "for i in [1]:\n  if 0:\n    print 10\n  else:\n    print 20\n  pass\nprint 9\n" `shouldBe` Right ["20", "9"]

  it "continues after multi-level dedent from while/for/if nesting" $ do
    runSource "x = 0\nwhile x < 1:\n  for i in [1]:\n    if i:\n      pass\n  x = x + 1\nprint x\nprint 9\n" `shouldBe` Right ["1", "9"]
 
  it "assigns string and prints identifier" $ do
    runSource "x = \"hello\"\nprint x\n" `shouldBe` Right ["hello"]

  it "concatenates strings and prints result" $ do
    runSource "print \"a\" + \"b\"\n" `shouldBe` Right ["ab"]

  it "evaluates multiplicative operators" $ do
    runSource "print 6 * 7\nprint 7 / 2\nprint 7 % 4\n" `shouldBe` Right ["42", "3", "3"]

  it "evaluates plus-assign statement" $ do
    runSource "x = 1\nx += 2\nprint x\n" `shouldBe` Right ["3"]

  it "evaluates minus-assign statement" $ do
    runSource "x = 5\nx -= 2\nprint x\n" `shouldBe` Right ["3"]

  it "evaluates star-assign statement" $ do
    runSource "x = 4\nx *= 3\nprint x\n" `shouldBe` Right ["12"]

  it "evaluates slash-assign statement" $ do
    runSource "x = 8\nx /= 2\nprint x\n" `shouldBe` Right ["4"]

  it "evaluates percent-assign statement" $ do
    runSource "x = 8\nx %= 3\nprint x\n" `shouldBe` Right ["2"]

  it "evaluates double-slash-assign statement" $ do
    runSource "x = 9\nx //= 2\nprint x\n" `shouldBe` Right ["4"]

  it "reports division by zero" $ do
    runSource "print 8 / 0\n" `shouldBe` Left "Value error: division by zero at 1:9"

  it "reports None-related type errors" $ do
    runSource "print len(None)\n" `shouldBe` Left "Type error: len expects string or list at 1:7"
    runSource "print None + 1\n" `shouldBe` Left "Type error: + expects int+int or string+string at 1:12"

  it "evaluates len builtin for string" $ do
    runSource "print len(\"abc\")\n" `shouldBe` Right ["3"]

  it "evaluates len builtin for list" $ do
    runSource "print len([1, 2])\n" `shouldBe` Right ["2"]

  it "evaluates bool builtin truthiness across core types" $ do
    runSource "print bool(0)\nprint bool(2)\nprint bool(None)\nprint bool(\"\")\nprint bool(\"x\")\nprint bool([])\nprint bool([1])\nprint bool({})\nprint bool({1: 2})\n" `shouldBe` Right ["0", "1", "0", "0", "1", "0", "1", "0", "1"]

  it "evaluates append builtin for list" $ do
    runSource "print append([1, 2], 3)\n" `shouldBe` Right ["[1, 2, 3]"]

  it "evaluates pop builtin for list" $ do
    runSource "print pop([1, 2])\n" `shouldBe` Right ["2"]

  it "evaluates keys builtin for dictionary" $ do
    runSource "print keys({1: 2, 3: 4})\n" `shouldBe` Right ["[1, 3]"]

  it "evaluates get builtin for dictionary" $ do
    runSource "print get({1: 2, 3: 4}, 3)\n" `shouldBe` Right ["4"]

  it "evaluates get builtin with default value" $ do
    runSource "print get({1: 2}, 9, 99)\nprint get({1: 2}, 1, 99)\n" `shouldBe` Right ["99", "2"]

  it "evaluates update builtin for dictionary" $ do
    runSource "print update({1: 2}, 1, 9)\nprint update({1: 2}, 3, 4)\n" `shouldBe` Right ["{1: 9}", "{1: 2, 3: 4}"]

  it "evaluates range builtin with start/stop and step" $ do
    runSource "print range(2, 5)\n" `shouldBe` Right ["[2, 3, 4]"]
    runSource "print range(5, 0, -2)\n" `shouldBe` Right ["[5, 3, 1]"]

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

  it "reports unsupported builtins outside MVP" $ do
    runSource "print clear([1])\n" `shouldBe` Left "Name error: undefined function clear at 1:7"

  it "keeps builtin call style as function form" $ do
    runSource "x = [1, 2]\nprint x.append(3)\n" `shouldBe` Left "UnexpectedCharacter '.'"

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

  it "allows function to read global variable" $ do
    runSource "x = 7\ndef readGlobal():\nreturn x\nprint readGlobal()\n" `shouldBe` Right ["7"]

  it "prefers function parameter over global variable" $ do
    runSource "x = 7\ndef echo(x):\nreturn x\nprint echo(99)\n" `shouldBe` Right ["99"]

  it "runs if/else and function return in one script" $ do
    runSource "x = 0\nif x:\nprint 1\nelse:\nprint 2\ndef id(v):\nreturn v\nprint id(7)\n" `shouldBe` Right ["2", "7"]

  it "runs if/elif/else and selects elif branch" $ do
    runSource "x = 0\ny = 1\nif x:\nprint 10\nelif y:\nprint 20\nelse:\nprint 30\n" `shouldBe` Right ["20"]

  it "skips while body when condition is false" $ do
    runSource "x = 0\nwhile x < 0:\nprint x\nprint 9\n" `shouldBe` Right ["9"]

  it "executes while loop body repeatedly until condition turns false" $ do
    runSource "x = 0\nwhile x < 3:\nx = x + 1\nprint x\n" `shouldBe` Right ["3"]
