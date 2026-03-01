module Test.VM.RunSourceVmSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp)" $ do
  it "supports import math with MVP functions" $ do
    runSourceVm "import math\nprint math.sqrt(9)\nprint math.sin(0)\nprint math.pi()\nprint math.e()\n" `shouldBe` Right ["3.0", "0.0", "3.141592653589793", "2.718281828459045"]

  it "supports import math with alias" $ do
    runSourceVm "import math as m\nprint m.sqrt(9)\nprint m.e()\n" `shouldBe` Right ["3.0", "2.718281828459045"]

  it "supports from math import with alias" $ do
    runSourceVm "from math import sqrt as s, pi\nprint s(9)\nprint pi\n" `shouldBe` Right ["3.0", "3.141592653589793"]

  it "handles arbitrary-size integer arithmetic" $ do
    runSourceVm "x = 123456789012345678901234567890\nprint x + 1\n" `shouldBe` Right ["123456789012345678901234567891"]

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
    runSourceVm "x @ 1\n" `shouldBe` Left "UnexpectedCharacter '@'"

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

  it "returns parse error for unsupported star expansion in call arguments" $ do
    runSourceVm "print f(*[1,2])\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 9})"

  it "returns parse error for unsupported double-star expansion in call arguments" $ do
    runSourceVm "print f(**{})\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 9})"

  it "returns parse error for unsupported *args in function definition" $ do
    runSourceVm "def f(*args):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported **kwargs in function definition" $ do
    runSourceVm "def f(**kwargs):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported keyword-only separator in function definition" $ do
    runSourceVm "def f(*, a):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported positional-only separator in function definition" $ do
    runSourceVm "def f(a, /):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 10})"

  it "returns parse error for unsupported type annotation in function parameter" $ do
    runSourceVm "def f(a: int):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 8})"

  it "returns parse error for unsupported return type annotation in function definition" $ do
    runSourceVm "def f() -> int:\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 9})"

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

  it "returns parse error for for header without suite body" $ do
    runSourceVm "for i in 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for standalone elif header" $ do
    runSourceVm "elif 1:\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for standalone else header" $ do
    runSourceVm "else:\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for unexpected leading indentation" $ do
    runSourceVm "  print 1\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 1})"

  it "returns parse error for if header followed by blank line only" $ do
    runSourceVm "if 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

  it "returns parse error for while header followed by blank line only" $ do
    runSourceVm "while 1:\n\n" `shouldBe` Left "ExpectedExpression (Position {line = 2, column = 1})"

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
