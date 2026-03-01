module Test.VM.RunSourceVmSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp)" $ do
  it "runs assignment and print with integer addition" $ do
    runSourceVm "x = 1\nprint x + 2\n" `shouldBe` Right ["3"]

  it "runs if/else using integer truthiness" $ do
    runSourceVm "x = 0\nif x:\n  print 1\nelse:\n  print 2\n" `shouldBe` Right ["2"]

  it "runs while loop with assignment in body" $ do
    runSourceVm "x = 1\nwhile x:\n  print x\n  x = 0\n" `shouldBe` Right ["1"]

  it "runs function definition and call with parameters" $ do
    runSourceVm "def add(a, b):\n  return a + b\nprint add(1, 2)\n" `shouldBe` Right ["3"]

  it "runs function definition and call with default parameters" $ do
    runSourceVm "def add(a, b = 2):\n  return a + b\nprint add(1)\nprint add(1, 3)\n" `shouldBe` Right ["3", "4"]

  it "runs function call with keyword arguments" $ do
    runSourceVm "def add(a, b):\n  return a + b\nprint add(a=1, b=2)\n" `shouldBe` Right ["3"]

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

  it "prioritizes duplicate keyword detection over second keyword evaluation error" $ do
    runSourceVm "def f(a):\n  return a\nprint f(a=1, a=len(1))\n" `shouldBe` Left "Argument error: duplicate keyword argument a at 3:14"

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
    runSourceVm "print bool()\n" `shouldBe` Left "Argument count mismatch when calling bool at 1:7"
    runSourceVm "print range(1, 2, 0)\n" `shouldBe` Left "Value error: range step must not be zero at 1:7"

  it "evaluates append sort and reverse builtins" $ do
    runSourceVm "print append([1, 2], 3)\nprint sort([3, 1.5, 2])\nprint reverse([1, 2, 3])\n" `shouldBe` Right ["[1, 2, 3]", "[1.5, 2, 3]", "[3, 2, 1]"]

  it "reports append sort and reverse builtin errors" $ do
    runSourceVm "print append(1, 2)\n" `shouldBe` Left "Type error: append expects list as first argument at 1:7"
    runSourceVm "print append([1])\n" `shouldBe` Left "Argument count mismatch when calling append at 1:7"
    runSourceVm "print sort([1, \"x\"])\n" `shouldBe` Left "Type error: sort expects list of number at 1:7"
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
