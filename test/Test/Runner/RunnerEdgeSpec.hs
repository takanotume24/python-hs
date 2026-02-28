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

  it "returns parse error for unsupported star expansion in call arguments" $ do
    runSource "print f(*[1,2])\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 9})"

  it "returns parse error for unsupported double-star expansion in call arguments" $ do
    runSource "print f(**{})\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 9})"

  it "returns parse error for unsupported *args in function definition" $ do
    runSource "def f(*args):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported **kwargs in function definition" $ do
    runSource "def f(**kwargs):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported keyword-only separator in function definition" $ do
    runSource "def f(*, a):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  it "returns parse error for unsupported positional-only separator in function definition" $ do
    runSource "def f(a, /):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 10})"

  it "returns parse error for unsupported type annotation in function parameter" $ do
    runSource "def f(a: int):\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 8})"

  it "returns parse error for unsupported return type annotation in function definition" $ do
    runSource "def f() -> int:\n  pass\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 9})"

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
