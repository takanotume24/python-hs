{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Runner.RunnerVmParityPart1Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource and runSourceVm parity (mvp subset) - core" $ do
  it "matches for assignment and print" $ do
    shouldMatchVm "x = 1\nprint x + 2\n"

  it "matches for if/else and while" $ do
    shouldMatchVm "x = 0\nif x:\n  print 1\nelse:\n  print 2\nwhile x:\n  print 3\n"

  it "matches for function call with return" $ do
    shouldMatchVm "def add(a, b):\n  return a + b\nprint add(1, 2)\n"

  it "matches for function call with default parameter" $ do
    shouldMatchVm "def add(a, b = 2):\n  return a + b\nprint add(1)\nprint add(1, 3)\n"

  it "matches for function call with keyword arguments" $ do
    shouldMatchVm "def add(a, b):\n  return a + b\nprint add(a=1, b=2)\n"

  it "matches for RunnerEdge function and scope behaviors" $ do
    shouldMatchVm "def add(a, b):\nprint a\nprint add(1, 2)\n"
    shouldMatchVm "def add(a, b = 2,):\n  return a + b\nprint add(3)\n"
    shouldMatchVm "def add(a, b = 2):\n  return a + b\nprint add()\n"
    shouldMatchVm "def add(a):\n  return a\nprint add(1, 2)\n"
    shouldMatchVm "def add(a, b):\n  return a + b\nprint add(a=1)\n"
    shouldMatchVm "x = 7\ndef readGlobal():\nreturn x\nprint readGlobal()\n"
    shouldMatchVm "x = 7\ndef echo(x):\nreturn x\nprint echo(99)\n"
    shouldMatchVm "def makeGlobal():\n  global y\n  y = 5\nprint makeGlobal()\nprint y\n"
    shouldMatchVm "x = 1\ndef setViaBranch():\n  if 0:\n    global x\n  x = 2\nprint setViaBranch()\nprint x\n"

  it "matches for RunnerEdge integration scripts" $ do
    shouldMatchVm "x = 0\nif x:\nprint 1\nelse:\nprint 2\ndef id(v):\nreturn v\nprint id(7)\n"
    shouldMatchVm "x = 0\ny = 1\nif x:\nprint 10\nelif y:\nprint 20\nelse:\nprint 30\n"
    shouldMatchVm "x = 0\nwhile x < 0:\nprint x\nprint 9\n"
    shouldMatchVm "x = 0\nwhile x < 3:\nx = x + 1\nprint x\n"
    shouldMatchVm "print update({}, 1)\n"

  it "matches for RunnerEdge early integration edges" $ do
    shouldMatchVm "print -2\n"
    shouldMatchVm "x = 3\nprint -x\nprint -(1 + 2)\n"
    shouldMatchVm "print -\"x\"\n"
    shouldMatchVm "print values({1: 2, 3: 4})\nprint items({1: 2, 3: 4})\n"
    shouldMatchVm "x = [1, 2]\nprint x.append(3)\n"
    shouldMatchVm "print 1.append(2)\n"
    shouldMatchVm "print 1.get(1)\n"
    shouldMatchVm "print {}.setdefault(1)\n"
    shouldMatchVm "print 1.setdefault(1)\n"
    shouldMatchVm "print {}.update(1)\n"
    shouldMatchVm "print len()\n"
    shouldMatchVm "print len(1, 2)\n"
    shouldMatchVm "print {}.update()\n"
    shouldMatchVm "print {}.setdefault()\n"
    shouldMatchVm "for i in range(3):\nprint i\n"
    shouldMatchVm "for i in [10, 20]:\nprint i\n"
    shouldMatchVm "for k in {3: 30, 1: 10}:\nprint k\n"

  it "matches for RunnerEdge exact source variants" $ do
    shouldMatchVm "def add(a, b = 2):\n  return a + b\nprint add(3)\nprint add(3, 4)\n"
    shouldMatchVm "def add(a, b):\n  return a + b\nprint add(1, b=2)\n"
    shouldMatchVm "def bad(a = 1, b):\n  return a + b\n"
    shouldMatchVm "def dup(a, a):\n  return a\n"
    shouldMatchVm "def dup(a = 1, a = 2):\n  return a\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(a=1)\n"
    shouldMatchVm "def f(a, b = 1 / 0):\n  return a + b\nprint f(a=len(1))\n"
    shouldMatchVm "for i in \"abc\":\nprint i\n"
    shouldMatchVm "for i in range(2000):\npass\nprint 1\n"
    shouldMatchVm "for i in range(2001):\npass\n"
    shouldMatchVm "print [1, 2]\n"
    shouldMatchVm "print {1: 2, 3: 4}\n"
    shouldMatchVm "print \"a\" == \"a\"\nprint \"a\" != \"b\"\n"
    shouldMatchVm "x = 0\nwhile x < 2000:\nx = x + 1\nprint x\n"
    shouldMatchVm "x = 0\nwhile x < 2001:\nx = x + 1\nprint x\n"
    shouldMatchVm "x = 10\ndef setGlobal():\n  global x\n  x = 99\n  return x\nprint setGlobal()\nprint x\n"

  it "matches for keyword argument error behaviors" $ do
    shouldMatchVm "def f(a):\n  return a\nprint f(a=1, a=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(b=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, a=2)\n"
    shouldMatchVm "print len(x=[1])\n"

  it "matches for keyword argument error precedence behaviors" $ do
    shouldMatchVm "print len(x=len(1))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(a=len(1), a=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(a=1, a=len(1))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(a=missing, a=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(a=1, a=missing)\n"
    shouldMatchVm "def f(a, b):\n  return a + b\nprint f(a=1, b=2, a=3, b=4)\n"
    shouldMatchVm "def f(a, b):\n  return a + b\nprint f(1, 2, b=3, a=4)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, a=len(1))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, a=missing)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, a=len(x=[1]))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, a=len(missing))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(z=1, b=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(b=len(1))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(b=missing)\n"
    shouldMatchVm "print len(x=missing)\n"
    shouldMatchVm "d = {}\nprint d.update(k=1)\n"
    shouldMatchVm "d = {}\nprint d.update(k=len(1))\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(b=1, b=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, b=2)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, z=2, b=3)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, 2, a=3)\n"
    shouldMatchVm "def f(a):\n  return a\nprint f(1, a=2, b=3)\n"

  it "matches for keyword-related parse boundary errors" $ do
    shouldMatchVm "print f(a=1, 2)\n"
    shouldMatchVm "print f(*[1,2])\n"
    shouldMatchVm "print f(**{})\n"
    shouldMatchVm "def f(*args):\n  pass\n"
    shouldMatchVm "def f(**kwargs):\n  pass\n"
    shouldMatchVm "def f(*, a):\n  pass\n"
    shouldMatchVm "def f(a, /):\n  pass\n"
    shouldMatchVm "def f(a: int):\n  pass\n"
    shouldMatchVm "def f() -> int:\n  pass\n"

  it "matches for varargs function execution" $ do
    shouldMatchVm "def f(*args):\n  return len(args)\nprint f(1, 2, 3)\n"
    shouldMatchVm "def f(**kwargs):\n  return get(kwargs, \"x\", 0)\nprint f(x=7)\n"
    shouldMatchVm "def f(*args, **kwargs):\n  return len(args) + get(kwargs, \"x\", 0)\nprint f(1, 2, x=3)\n"
    shouldMatchVm "def f(*args):\n  print args\nf(1, 2, 3)\n"
    shouldMatchVm "def f(**kwargs):\n  print kwargs\nf(x=7)\n"

  it "matches for walrus operator execution" $ do
    shouldMatchVm "print (x := 5)\nprint x\n"
    shouldMatchVm "if (n := 1):\n  print n\n"
    shouldMatchVm "print [x for x in [1, 2, 3, 4] if (y := x % 2) == 0]\n"
    shouldMatchVm "i = 0\nwhile (n := i) < 3:\n  print n\n  i = i + 1\n"
    shouldMatchVm "print (x := 2) + 1\nprint x\n"
    shouldMatchVm "print [x for x in [1, 2] if (y := x) == 1]\nprint y\n"

  it "matches for list comprehension execution" $ do
    shouldMatchVm "print [k for k in {1: 10, 2: 20}]\n"
    shouldMatchVm "print [x for x in (1, 2)]\n"
    shouldMatchVm "print [x for x in 5]\n"
    shouldMatchVm "print [x for x in 'abc']\n"
    shouldMatchVm "def f():\n  return [x * 2 for x in [1, 2]]\nprint f()\n"
    shouldMatchVm "print [[y for y in [x, x + 1]] for x in [1, 2]]\n"

  it "matches for non-keyword parse boundary errors" $ do
    shouldMatchVm "print\n"
    shouldMatchVm "global\n"
    shouldMatchVm "if 1:\n"
    shouldMatchVm "def f():\n"
    shouldMatchVm "while 1:\n"
    shouldMatchVm "for i in 1:\n"
    shouldMatchVm "elif 1:\n"
    shouldMatchVm "else:\n"
    shouldMatchVm "  print 1\n"
    shouldMatchVm "if 1:\n\n"
    shouldMatchVm "while 1:\n\n"
    shouldMatchVm "if 1\n"
    shouldMatchVm "while 1\n"
    shouldMatchVm "for i in 1\n"
    shouldMatchVm "def f()\n"

  it "matches for default expression conflict and precedence behaviors" $ do
    shouldMatchVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1, a=2, b=3)\n"
    shouldMatchVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1, b=2, d=3)\n"
    shouldMatchVm "def f(a, b = 1 / 0):\n  return a + b\nprint f(1)\n"
    shouldMatchVm "def f(a, b = len(x=[1])):\n  return a + b\nprint f(a=len(1))\n"
    shouldMatchVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(1)\n"
    shouldMatchVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(1, a=2, b=3)\n"
    shouldMatchVm "def f(a, b = missing):\n  return a + b\nprint f(1)\n"
    shouldMatchVm "def f(a, b = len(1)):\n  return a + b\nprint f(1)\n"
    shouldMatchVm "def f(a, b = 2, c = d):\n  return a + b + c\nprint f(1)\n"
    shouldMatchVm "def f(a, b = c, c = 2):\n  return a + b + c\nprint f(1)\n"
    shouldMatchVm "def f(a, b = 2, c = b + d):\n  return a + b + c\nprint f(1)\n"
    shouldMatchVm "def f(a, b = len(a)):\n  return a + b\nprint f(1)\n"
    shouldMatchVm "def f(a, b = 1 / 0):\n  return a + b\nprint f(len(1))\n"
    shouldMatchVm "def f(a, b = len(x=[1])):\n  return a + b\nprint f(len(1))\n"
    shouldMatchVm "def f(a, b, c = len(x=[1])):\n  return a + b + c\nprint f(a=1)\n"
    shouldMatchVm "def f(a, b):\n  return a + b\nprint f(len(1), b=len(1))\n"
    shouldMatchVm "def f(a, b):\n  return a + b\nprint f(a=len(1), b=len(1))\n"
    shouldMatchVm "def f(a, b):\n  return a + b\nprint f(a=1, b=len(1))\n"
    shouldMatchVm "def f(a, b):\n  return a + b\nprint f(1, b=len(1))\n"
    shouldMatchVm "def f(a, b = len([a, missing])):\n  return a + b\nprint f(1)\n"

  it "matches for trailing commas in function definition and call" $ do
    shouldMatchVm "def add(a, b,):\n  return a + b\nprint add(1, 2,)\n"

  it "matches for for-loop over int/list/dict" $ do
    shouldMatchVm "for i in 3:\n  print i\nfor x in [10, 20]:\n  print x\nfor k in {3: 30, 1: 10}:\n  print k\n"
  where
    shouldMatchVm source = runSourceVm source `shouldBe` runSource source
