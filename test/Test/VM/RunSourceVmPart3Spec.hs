{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.VM.RunSourceVmPart3Spec (spec) where

import Data.Either (isLeft)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 3" $ do
  it "supports deleting instance attribute" $ do
    runSourceVm "class A:\n  x = 1\na = A()\na.x = 2\ndel a.x\nprint a.x\n" `shouldBe` Right ["1"]

  it "supports deleting class attribute through classmethod" $ do
    runSourceVm "class A:\n  x = 1\n  @classmethod\n  def remove(cls):\n    del cls.x\n_ = A.remove()\nprint A.x\n" `shouldBe` Left "Attribute error: undefined attribute A.x at 7:7"

  it "reports UnboundLocalError for local variable referenced before assignment" $ do
    runSourceVm "x = 1\ndef f():\n  return x\n  x = 2\nprint f()\n" `shouldBe` Left "UnboundLocalError: local variable x referenced before assignment at 3:10"

  it "reports UnboundLocalError after deleting a local variable inside function" $ do
    runSourceVm "def f():\n  x = 1\n  del x\n  return x\nresult = f()\nprint result\n" `shouldBe` Left "UnboundLocalError: local variable x referenced before assignment at 4:10"

  it "unbinds exception alias after except block at module level" $ do
    runSourceVm "try:\n  raise 1\nexcept RuntimeError as e:\n  pass\nprint e\n" `shouldBe` Left "Name error: undefined identifier e at 5:7"

  it "unbinds exception alias after except block inside function" $ do
    runSourceVm "def f():\n  try:\n    raise 1\n  except RuntimeError as e:\n    pass\n  return e\nresult = f()\nprint result\n" `shouldBe` Left "UnboundLocalError: local variable e referenced before assignment at 6:10"

  it "runs function decorators in bottom-up order" $ do
    runSourceVm "def add1(fn):\n  return lambda x: (fn)(x) + 1\ndef mul2(fn):\n  return lambda x: (fn)(x) * 2\n@add1\n@mul2\ndef f(x):\n  return x\nprint f(3)\n" `shouldBe` Right ["7"]

  it "runs call-style decorators" $ do
    runSourceVm "def add(n):\n  return lambda fn: (lambda x: (fn)(x) + n)\n@add(2)\ndef f(x):\n  return x\nprint f(3)\n" `shouldBe` Right ["5"]

  it "runs class decorators" $ do
    runSourceVm "x = 0\ndef deco(cls):\n  global x\n  x = 1\n  return cls\n@deco\nclass C:\n  pass\nprint x\n" `shouldBe` Right ["1"]

  it "runs dataclass core behavior (__init__/__repr__/__eq__)" $ do
    runSourceVm "from dataclasses import dataclass\n@dataclass\nclass Point:\n  x: int\n  y: int\np1 = Point(1, 2)\np2 = Point(1, 2)\nprint p1.x\nprint p1.__repr__()\nprint p1 == p2\n" `shouldBe` Right ["1", "Point(x=1, y=2)", "1"]

  it "runs dataclass order comparisons" $ do
    runSourceVm "from dataclasses import dataclass\n@dataclass(order=1)\nclass Point:\n  x: int\n  y: int\nprint Point(1, 2) < Point(2, 1)\nprint Point(2, 1) > Point(1, 2)\n" `shouldBe` Right ["1", "1"]

  it "runs dataclass field default_factory per instance" $ do
    runSourceVm "from dataclasses import dataclass, field\n@dataclass\nclass Box:\n  items: list = field(default_factory=list)\nb1 = Box()\nb2 = Box()\nb1.items = append(b1.items, 1)\nprint b1.items\nprint b2.items\n" `shouldBe` Right ["[1]", "[]"]

  it "rejects assignment to frozen dataclass field" $ do
    runSourceVm "from dataclasses import dataclass\n@dataclass(frozen=1)\nclass Point:\n  x: int\np = Point(1)\np.x = 2\n" `shouldBe` Left "Type error: cannot assign to frozen dataclass field x at 0:0"

  it "runs tuple literal, singleton tuple, len, bool, and for iteration" $ do
    runSourceVm "print (1, 2)\nprint (1,)\nprint len((1, 2, 3))\nprint bool(())\nfor x in (3, 4):\n  print x\n" `shouldBe` Right ["(1, 2)", "(1,)", "3", "0", "3", "4"]

  it "runs tuple comparison and indexing/slicing" $ do
    runSourceVm "print (1, 2) == (1, 2)\nprint (1, 2) < (1, 3)\nprint (1, 2, 3)[0]\nprint (1, 2, 3)[1:]\nprint (1, 2, 3)[:2]\n" `shouldBe` Right ["1", "1", "1", "(2, 3)", "(1, 2)"]

  it "runs tuple unpacking assignment and tuple match sequence pattern" $ do
    runSourceVm "a, b = (1, 2)\nprint a\nprint b\nx = (7, 8)\nmatch x:\n  case [p, q]:\n    print p\n    print q\n  case _:\n    print 0\n" `shouldBe` Right ["1", "2", "7", "8"]

  it "runs generator function with yield in for loop" $ do
    runSourceVm "def gen():\n  yield 1\n  yield 2\nfor x in gen():\n  print x\n" `shouldBe` Right ["1", "2"]

  it "runs yield from delegation" $ do
    runSourceVm "def inner():\n  yield 1\n  yield 2\ndef outer():\n  yield from inner()\n  yield 3\nfor x in outer():\n  print x\n" `shouldBe` Right ["1", "2", "3"]

  it "runs lambda expression call via variable" $ do
    runSourceVm "f = lambda x: x + 1\nprint f(2)\n" `shouldBe` Right ["3"]

  it "runs lambda with default parameter" $ do
    runSourceVm "f = lambda x, y=1: x + y\nprint f(2)\nprint f(2, 3)\n" `shouldBe` Right ["3", "5"]

  it "runs immediate lambda invocation" $ do
    runSourceVm "print (lambda x: x + 1)(2)\n" `shouldBe` Right ["3"]

  it "runs lambda closure capture from outer scope" $ do
    runSourceVm "def makeAdder(n):\n  return lambda x: x + n\nadd2 = makeAdder(2)\nprint add2(3)\n" `shouldBe` Right ["5"]

  it "runs list comprehension with single for clause" $ do
    runSourceVm "print [x * 2 for x in [1, 2, 3]]\n" `shouldBe` Right ["[2, 4, 6]"]

  it "runs list comprehension with if clause" $ do
    runSourceVm "print [x for x in [1, 2, 3, 4] if x % 2 == 0]\n" `shouldBe` Right ["[2, 4]"]

  it "runs list comprehension with nested for clauses" $ do
    runSourceVm "print [x + y for x in [1, 2] for y in [10, 20]]\n" `shouldBe` Right ["[11, 21, 12, 22]"]

  it "runs list comprehension with chained if clauses" $ do
    runSourceVm "print [x for x in [1, 2, 3, 4, 5, 6] if x % 2 == 0 if x > 2]\n" `shouldBe` Right ["[4, 6]"]

  it "runs list comprehension with unpack target" $ do
    runSourceVm "print [a + b for a, b in [[1, 10], [2, 20]]]\n" `shouldBe` Right ["[11, 22]"]

  it "runs list comprehension with walrus condition" $ do
    runSourceVm "print [x for x in [1, 2, 3, 4] if (y := x % 2) == 0]\n" `shouldBe` Right ["[2, 4]"]

  it "does not leak comprehension variable to outer scope" $ do
    runSourceVm "x = 100\nprint [x for x in [1, 2]]\nprint x\n" `shouldBe` Right ["[1, 2]", "100"]

  it "runs function definition and call with default parameters" $ do
    runSourceVm "def add(a, b = 2):\n  return a + b\nprint add(1)\nprint add(1, 3)\n" `shouldBe` Right ["3", "4"]

  it "runs function call with keyword arguments" $ do
    runSourceVm "def add(a, b):\n  return a + b\nprint add(a=1, b=2)\n" `shouldBe` Right ["3"]

  it "mirrors RunnerEdge: function body print and call output sequence" $ do
    runSourceVm "def add(a, b):\nprint a\nprint add(1, 2)\n" `shouldBe` Right ["1", "0"]

  it "mirrors RunnerEdge: default parameter with trailing comma" $ do
    runSourceVm "def add(a, b = 2,):\n  return a + b\nprint add(3)\n" `shouldBe` Right ["5"]

  it "mirrors RunnerEdge: count mismatch when required parameter missing with defaults" $ do
    runSourceVm "def add(a, b = 2):\n  return a + b\nprint add()\n" `shouldBe` Left "Argument count mismatch when calling add at 3:7"

  it "mirrors RunnerEdge: count mismatch at call-site for too many positional args" $ do
    runSourceVm "def add(a):\n  return a\nprint add(1, 2)\n" `shouldBe` Left "Argument count mismatch when calling add at 3:7"

  it "mirrors RunnerEdge: count mismatch at call-site for missing required keyword set" $ do
    runSourceVm "def add(a, b):\n  return a + b\nprint add(a=1)\n" `shouldBe` Left "Argument count mismatch when calling add at 3:7"

  it "mirrors RunnerEdge: function reads global variable" $ do
    runSourceVm "x = 7\ndef readGlobal():\nreturn x\nprint readGlobal()\n" `shouldBe` Right ["7"]

  it "mirrors RunnerEdge: function parameter shadows global" $ do
    runSourceVm "x = 7\ndef echo(x):\nreturn x\nprint echo(99)\n" `shouldBe` Right ["99"]

  it "mirrors RunnerEdge: creates new global variable via global declaration" $ do
    runSourceVm "def makeGlobal():\n  global y\n  y = 5\nprint makeGlobal()\nprint y\n" `shouldBe` Right ["0", "5"]

  it "mirrors RunnerEdge: global declaration in branch is function-wide" $ do
    runSourceVm "x = 1\ndef setViaBranch():\n  if 0:\n    global x\n  x = 2\nprint setViaBranch()\nprint x\n" `shouldBe` Right ["0", "2"]

  it "mirrors RunnerEdge: if/else and function return in one script" $ do
    runSourceVm "x = 0\nif x:\nprint 1\nelse:\nprint 2\ndef id(v):\nreturn v\nprint id(7)\n" `shouldBe` Right ["2", "7"]

  it "mirrors RunnerEdge: if/elif/else selects elif branch" $ do
    runSourceVm "x = 0\ny = 1\nif x:\nprint 10\nelif y:\nprint 20\nelse:\nprint 30\n" `shouldBe` Right ["20"]

  it "mirrors RunnerEdge: while loop skipped when condition false" $ do
    runSourceVm "x = 0\nwhile x < 0:\nprint x\nprint 9\n" `shouldBe` Right ["9"]

  it "mirrors RunnerEdge: while loop repeats until condition turns false" $ do
    runSourceVm "x = 0\nwhile x < 3:\nx = x + 1\nprint x\n" `shouldBe` Right ["3"]

  it "mirrors RunnerEdge: update merge type error for non-dict second argument" $ do
    runSourceVm "print update({}, 1)\n" `shouldBe` Left "Type error: update expects dict as second argument at 1:7"

  it "mirrors RunnerEdge: unary minus literal and expression forms" $ do
    runSourceVm "print -2\n" `shouldBe` Right ["-2"]
    runSourceVm "x = 3\nprint -x\nprint -(1 + 2)\n" `shouldBe` Right ["-3", "-3"]

  it "mirrors RunnerEdge: unary minus type error" $ do
    runSourceVm "print -\"x\"\n" `shouldBe` Left "Type error: unary - expects int at 1:7"

  it "mirrors RunnerEdge: values/items dictionary builtins" $ do
    runSourceVm "print values({1: 2, 3: 4})\nprint items({1: 2, 3: 4})\n" `shouldBe` Right ["[2, 4]", "[[1, 2], [3, 4]]"]

  it "mirrors RunnerEdge: method-call builtin behaviors and positions" $ do
    runSourceVm "x = [1, 2]\nprint x.append(3)\n" `shouldBe` Right ["[1, 2, 3]"]
    runSourceVm "print 1.append(2)\n" `shouldBe` Left "Type error: append expects list as first argument at 1:9"
    runSourceVm "print 1.get(1)\n" `shouldBe` Left "Type error: get expects dict as first argument at 1:9"
    runSourceVm "print {}.setdefault(1)\n" `shouldBe` Right ["{1: None}"]
    runSourceVm "print 1.setdefault(1)\n" `shouldBe` Left "Type error: setdefault expects dict as first argument at 1:9"
    runSourceVm "print {}.update(1)\n" `shouldBe` Left "Type error: update expects dict as second argument at 1:10"

  it "mirrors RunnerEdge: builtin count mismatch at call-site positions" $ do
    runSourceVm "print len()\n" `shouldBe` Left "Argument count mismatch when calling len at 1:7"
    runSourceVm "print len(1, 2)\n" `shouldBe` Left "Argument count mismatch when calling len at 1:7"
    runSourceVm "print {}.update()\n" `shouldBe` Left "Argument count mismatch when calling update at 1:10"
    runSourceVm "print {}.setdefault()\n" `shouldBe` Left "Argument count mismatch when calling setdefault at 1:10"
