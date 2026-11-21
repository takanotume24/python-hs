{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Runner.RunnerCorePart1Spec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration core) - part 1" $ do
  it "runs a simple script (assignment + print)" $ do
    runSource "x = 1\nprint x\n" `shouldBe` Right ["1"]

  it "accepts trailing commas in function definition and call" $ do
    runSource "def add(a, b,):\n  return a + b\nprint add(1, 2,)\n" `shouldBe` Right ["3"]

  it "evaluates keyword-only call arguments left-to-right" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b):\n  return a + b\nprint add(a=probe(1), b=probe(2))\n" `shouldBe` Right ["1", "2", "3"]

  it "evaluates mixed positional and keyword call arguments left-to-right" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b):\n  return a + b\nprint add(probe(1), b=probe(2))\n" `shouldBe` Right ["1", "2", "3"]

  it "evaluates explicit args before missing default expressions" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(2), c=probe(3)):\n  return a + b + c\nprint add(a=probe(1), c=probe(4))\n" `shouldBe` Right ["1", "4", "2", "7"]

  it "evaluates keyword argument before default expression that references it" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(a)):\n  return a + b\nprint add(a=probe(1))\n" `shouldBe` Right ["1", "1", "2"]

  it "evaluates positional and keyword arguments before omitted default expressions in mixed call" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(a), c=probe(30)):\n  return a + b + c\nprint add(probe(1), c=probe(3))\n" `shouldBe` Right ["1", "3", "1", "5"]

  it "evaluates default expression using global value at call time" $ do
    runSource "x = 10\ndef add(a, b=x):\n  return a + b\nx = 20\nprint add(1)\n" `shouldBe` Right ["21"]

  it "evaluates default expression referencing earlier default parameter" $ do
    runSource "def add(a, b=2, c=b):\n  return a + b + c\nprint add(1)\n" `shouldBe` Right ["5"]

  it "evaluates default expression referencing explicit argument parameter" $ do
    runSource "def add(a, b=2, c=a):\n  return a + b + c\nprint add(3)\n" `shouldBe` Right ["8"]

  it "evaluates default expression referencing parameter bound by keyword argument" $ do
    runSource "def add(a, b=a):\n  return a + b\nprint add(a=3)\n" `shouldBe` Right ["6"]

  it "evaluates composite default expression referencing bound values" $ do
    runSource "def add(a, b=2, c=b+a):\n  return a + b + c\nprint add(3)\n" `shouldBe` Right ["10"]

  it "evaluates builtin call in default expression using bound values" $ do
    runSource "def add(a, b=2, c=len([a, b])):\n  return a + b + c\nprint add(3)\n" `shouldBe` Right ["7"]

  it "evaluates builtin default expression with side effect when omitted" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=2, c=probe(len([a, b]))):\n  return a + b + c\nprint add(3)\n" `shouldBe` Right ["2", "7"]

  it "does not evaluate builtin default expression with side effect when explicitly overridden" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=2, c=probe(len([a, b]))):\n  return a + b + c\nprint add(3, c=probe(9))\n" `shouldBe` Right ["9", "14"]

  it "does not evaluate default expression side effect when argument is explicitly provided" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(2)):\n  return a + b\nprint add(1, b=3)\n" `shouldBe` Right ["4"]

  it "evaluates default expression side effect when argument is omitted" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(2)):\n  return a + b\nprint add(1)\n" `shouldBe` Right ["2", "3"]

  it "evaluates multiple omitted default expressions in parameter order" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(2), c=probe(3)):\n  return a + b + c\nprint add(1)\n" `shouldBe` Right ["2", "3", "6"]

  it "evaluates omitted defaults in parameter order when explicit and omitted arguments are interleaved" $ do
    runSource "def probe(x):\n  print x\n  return x\ndef add(a, b=probe(2), c=probe(30), d=probe(4)):\n  return a + b + c + d\nprint add(a=probe(1), c=probe(3))\n" `shouldBe` Right ["1", "3", "2", "4", "10"]

  it "returns None for bare return in function" $ do
    runSource "def f():\n  return\nprint f()\n" `shouldBe` Right ["None"]

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

  it "parses and evaluates float literal forms" $ do
    runSource "print 1.\nprint .5\nprint 1e3\nprint 1.2e-3\n" `shouldBe` Right ["1.0", "0.5", "1000.0", "1.2e-3"]

  it "accepts trailing commas in list and dict literals" $ do
    runSource "print [1, 2,]\nprint {1: 2,}\n" `shouldBe` Right ["[1, 2]", "{1: 2}"]

  it "concatenates strings and prints result" $ do
    runSource "print \"a\" + \"b\"\n" `shouldBe` Right ["ab"]
