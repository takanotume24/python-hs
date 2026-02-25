module Test.Runner.RunnerCoreSpec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration core)" $ do
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
    runSource "print 6 * 7\nprint 7 / 2\nprint 7 // 2\nprint 7 % 4\nprint 7 - 4\n" `shouldBe` Right ["42", "3", "3", "3", "3"]

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

  it "evaluates sort builtin for integer list" $ do
    runSource "print sort([3, 1, 2])\nprint sort([])\n" `shouldBe` Right ["[1, 2, 3]", "[]"]

  it "evaluates reverse builtin for list" $ do
    runSource "print reverse([1, 2, 3])\nprint reverse([])\n" `shouldBe` Right ["[3, 2, 1]", "[]"]

  it "reports reverse builtin type and argument errors" $ do
    runSource "print reverse(1)\n" `shouldBe` Left "Type error: reverse expects list as first argument at 1:7"
    runSource "print reverse([], 1)\n" `shouldBe` Left "Argument count mismatch when calling reverse at 1:7"

  it "reports sort builtin type and argument errors" $ do
    runSource "print sort(1)\n" `shouldBe` Left "Type error: sort expects list as first argument at 1:7"
    runSource "print sort([\"x\"])\n" `shouldBe` Left "Type error: sort expects list of int at 1:7"
    runSource "print sort([], 1)\n" `shouldBe` Left "Argument count mismatch when calling sort at 1:7"

  it "evaluates remove builtin for list" $ do
    runSource "print remove([1, 2, 2], 2)\n" `shouldBe` Right ["[1, 2]"]

  it "reports remove builtin type/value/argument errors" $ do
    runSource "print remove(1, 2)\n" `shouldBe` Left "Type error: remove expects list as first argument at 1:7"
    runSource "print remove([1], 9)\n" `shouldBe` Left "Value error: remove value not found at 1:7"
    runSource "print remove([])\n" `shouldBe` Left "Argument count mismatch when calling remove at 1:7"

  it "evaluates insert builtin for list" $ do
    runSource "print insert([1, 3], 1, 2)\nprint insert([1], -1, 0)\nprint insert([1], 9, 2)\n" `shouldBe` Right ["[1, 2, 3]", "[0, 1]", "[1, 2]"]

  it "reports insert builtin type and argument errors" $ do
    runSource "print insert(1, 0, 2)\n" `shouldBe` Left "Type error: insert expects list as first argument at 1:7"
    runSource "print insert([], \"x\", 2)\n" `shouldBe` Left "Type error: insert expects int index at 1:7"
    runSource "print insert([], 0)\n" `shouldBe` Left "Argument count mismatch when calling insert at 1:7"

  it "evaluates pop builtin for list" $ do
    runSource "print pop([1, 2])\n" `shouldBe` Right ["2"]

  it "evaluates pop builtin for dictionary with optional default" $ do
    runSource "print pop({1: 2, 3: 4}, 3)\nprint pop({1: 2}, 9, 99)\n" `shouldBe` Right ["4", "99"]

  it "evaluates clear builtin for list and dictionary" $ do
    runSource "print clear([1, 2])\nprint clear({1: 2})\n" `shouldBe` Right ["[]", "{}"]

  it "reports clear builtin type and argument errors" $ do
    runSource "print clear(1)\n" `shouldBe` Left "Type error: clear expects list or dict at 1:7"
    runSource "print clear([], 1)\n" `shouldBe` Left "Argument count mismatch when calling clear at 1:7"

  it "evaluates keys builtin for dictionary" $ do
    runSource "print keys({1: 2, 3: 4})\n" `shouldBe` Right ["[1, 3]"]

  it "evaluates get builtin for dictionary" $ do
    runSource "print get({1: 2, 3: 4}, 3)\n" `shouldBe` Right ["4"]

  it "evaluates get builtin with default value" $ do
    runSource "print get({1: 2}, 9, 99)\nprint get({1: 2}, 1, 99)\n" `shouldBe` Right ["99", "2"]

  it "evaluates update builtin for dictionary" $ do
    runSource "print update({1: 2}, 1, 9)\nprint update({1: 2}, 3, 4)\n" `shouldBe` Right ["{1: 9}", "{1: 2, 3: 4}"]

  it "evaluates setdefault builtin for dictionary" $ do
    runSource "print setdefault({1: 2}, 1, 9)\nprint setdefault({1: 2}, 3, 4)\n" `shouldBe` Right ["{1: 2}", "{1: 2, 3: 4}"]

  it "reports setdefault builtin type and argument errors" $ do
    runSource "print setdefault(1, 2, 3)\n" `shouldBe` Left "Type error: setdefault expects dict as first argument at 1:7"
    runSource "print setdefault({}, 1)\n" `shouldBe` Left "Argument count mismatch when calling setdefault at 1:7"

  it "evaluates range builtin with start/stop and step" $ do
    runSource "print range(2, 5)\n" `shouldBe` Right ["[2, 3, 4]"]
    runSource "print range(5, 0, -2)\n" `shouldBe` Right ["[5, 3, 1]"]
