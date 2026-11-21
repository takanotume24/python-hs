{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.VM.RunSourceVmPart2Spec (spec) where

import Data.Either (isLeft)
import PythonHS.RunSourceVm (runSourceVm)
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 2" $ do
  it "catches error raised inside function call" $ do
    runSourceVm "def f():\n  raise \"boom\"\ntry:\n  f()\nexcept:\n  print 1\n" `shouldBe` Right ["1"]

  it "returns normally from function with try/except" $ do
    runSourceVm "def f():\n  try:\n    return 1\n  except:\n    return 2\nprint f()\n" `shouldBe` Right ["1"]

  it "matches value patterns and wildcard in order" $ do
    runSourceVm "x = 2\nmatch x:\n  case 1:\n    print 10\n  case 2:\n    print 20\n  case _:\n    print 30\n" `shouldBe` Right ["20"]

  it "supports OR pattern and guard" $ do
    runSourceVm "x = 1\nok = 1\nmatch x:\n  case 1 | 2 if ok:\n    print 7\n  case _:\n    print 9\n" `shouldBe` Right ["7"]

  it "supports sequence and mapping pattern captures" $ do
    runSourceVm "x = [1, 2, 3]\nmatch x:\n  case [a, b, *rest]:\n    print a\n    print b\n    print rest\n  case _:\n    print 0\nm = {\"k\": 8}\nmatch m:\n  case {\"k\": v}:\n    print v\n  case _:\n    print 0\n" `shouldBe` Right ["1", "2", "[3]", "8"]

  it "supports as-pattern captures in match" $ do
    runSourceVm "x = [4, 5]\nmatch x:\n  case [a, b] as pair:\n    print a\n    print b\n    print pair\n  case _:\n    print 0\n" `shouldBe` Right ["4", "5", "[4, 5]"]

  it "supports mapping pattern double-star rest capture" $ do
    runSourceVm "x = {\"k\": 8, \"x\": 9}\nmatch x:\n  case {\"k\": v, **rest}:\n    print v\n    print rest\n  case _:\n    print 0\n" `shouldBe` Right ["8", "{x: 9}"]

  it "supports import math with MVP functions" $ do
    runSourceVm "import math\nprint math.sqrt(9)\nprint math.sin(0)\nprint math.pi()\nprint math.e()\n" `shouldBe` Right ["3.0", "0.0", "3.141592653589793", "2.718281828459045"]

  it "supports import math with alias" $ do
    runSourceVm "import math as m\nprint m.sqrt(9)\nprint m.e()\n" `shouldBe` Right ["3.0", "2.718281828459045"]

  it "supports from math import with alias" $ do
    runSourceVm "from math import sqrt as s, pi\nprint s(9)\nprint pi\n" `shouldBe` Right ["3.0", "3.141592653589793"]

  it "does not bind bare submodule name for import pkg.sub" $
    withSystemTempDirectory "vm-local-submodule-import" $ \dir -> do
      let packageDir = dir </> "pkg"
      let initPath = packageDir </> "__init__.py"
      let subPath = packageDir </> "sub.py"
      createDirectory packageDir
      writeFile initPath "pass\n"
      writeFile subPath "def inc(x):\n  return x + 1\n"
      result <- runSourceVmWithSearchPaths (RunSourceVmWithSearchPathsConfig [dir] "import pkg.sub\nprint sub.inc(4)\n")
      result `shouldBe` Left "Name error: undefined identifier sub at 2:7"

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

  it "runs class init and method call" $ do
    runSourceVm "class A:\n  def __init__(self, v):\n    self.v = v\n  def get(self):\n    return self.v\nx = A(3)\nprint x.get()\n" `shouldBe` Right ["3"]

  it "runs single inheritance method lookup" $ do
    runSourceVm "class A:\n  def f(self):\n    return 7\nclass B(A):\n  pass\nx = B()\nprint x.f()\n" `shouldBe` Right ["7"]

  it "supports bound method value call via attribute lookup" $ do
    runSourceVm "class A:\n  def add(self, v):\n    return v + 1\nx = A()\nf = x.add\nprint f(2)\n" `shouldBe` Right ["3"]

  it "supports inherited bound method value call via attribute lookup" $ do
    runSourceVm "class A:\n  def f(self):\n    return 9\nclass B(A):\n  pass\nx = B()\ng = x.f\nprint g()\n" `shouldBe` Right ["9"]

  it "supports unbound method value call via class attribute lookup" $ do
    runSourceVm "class A:\n  def add(self, v):\n    return v + 1\nf = A.add\nprint f(A(), 2)\n" `shouldBe` Right ["3"]

  it "supports inherited unbound method value call via class attribute lookup" $ do
    runSourceVm "class A:\n  def f(self):\n    return 7\nclass B(A):\n  pass\ng = B.f\nprint g(B())\n" `shouldBe` Right ["7"]

  it "supports @staticmethod method call via class and instance" $ do
    runSourceVm "class A:\n  @staticmethod\n  def f(x): return x + 1\nprint A.f(2)\na = A()\ng = a.f\nprint g(3)\n" `shouldBe` Right ["3", "4"]

  it "supports @classmethod method call via class and instance" $ do
    runSourceVm "class A:\n  @classmethod\n  def f(cls, x): return x + 1\nprint A.f(2)\na = A()\ng = a.f\nprint g(3)\n" `shouldBe` Right ["3", "4"]

  it "supports class attributes referenced from class" $ do
    runSourceVm "class A:\n  x = 1\nprint A.x\n" `shouldBe` Right ["1"]

  it "supports class attributes as fallback from instance" $ do
    runSourceVm "class A:\n  x = 1\na = A()\nprint a.x\n" `shouldBe` Right ["1"]

  it "supports class attribute assignment" $ do
    runSourceVm "class A:\n  x = 1\nA.x = 2\nprint A.x\n" `shouldBe` Right ["2"]

  it "supports inherited class attributes referenced from subclass" $ do
    runSourceVm "class A:\n  x = 1\nclass B(A):\n  pass\nprint B.x\n" `shouldBe` Right ["1"]

  it "supports inherited class attributes as fallback from instance" $ do
    runSourceVm "class A:\n  x = 1\nclass B(A):\n  pass\nb = B()\nprint b.x\n" `shouldBe` Right ["1"]

  it "supports subclass overriding inherited class attributes" $ do
    runSourceVm "class A:\n  x = 1\nclass B(A):\n  x = 2\nprint B.x\nb = B()\nprint b.x\n" `shouldBe` Right ["2", "2"]

  it "supports class attribute access from instance method" $ do
    runSourceVm "class A:\n  x = 1\n  def get_x(self):\n    return self.x\na = A()\nprint a.get_x()\n" `shouldBe` Right ["1"]

  it "supports class attribute access from classmethod" $ do
    runSourceVm "class A:\n  x = 1\n  @classmethod\n  def get_x(cls):\n    return cls.x\nprint A.get_x()\na = A()\nprint a.get_x()\n" `shouldBe` Right ["1", "1"]

  it "supports inherited class attribute access from method" $ do
    runSourceVm "class A:\n  x = 1\nclass B(A):\n  def get_x(self):\n    return self.x\nb = B()\nprint b.get_x()\n" `shouldBe` Right ["1"]

  it "supports overridden class attribute access from method" $ do
    runSourceVm "class A:\n  x = 1\nclass B(A):\n  x = 2\n  def get_x(self):\n    return self.x\nb = B()\nprint b.get_x()\n" `shouldBe` Right ["2"]

  it "supports instance attribute shadowing class attribute" $ do
    runSourceVm "class A:\n  x = 1\na = A()\na.x = 2\nprint a.x\nprint A.x\n" `shouldBe` Right ["2", "1"]

  it "does not affect other instances when shadowing class attribute" $ do
    runSourceVm "class A:\n  x = 1\na = A()\nb = A()\na.x = 2\nprint a.x\nprint b.x\nprint A.x\n" `shouldBe` Right ["2", "1", "1"]

  it "supports instance attribute assignment without class attribute" $ do
    runSourceVm "class A:\n  pass\na = A()\na.x = 1\nprint a.x\n" `shouldBe` Right ["1"]

  it "shares class attribute reassignment across instances" $ do
    runSourceVm "class A:\n  xs = []\nA.xs = append(A.xs, 1)\na = A()\nb = A()\nprint a.xs\nprint b.xs\nprint A.xs\n" `shouldBe` Right ["[1]", "[1]", "[1]"]

  it "shares class attribute reassignment through classmethod" $ do
    runSourceVm "class A:\n  xs = []\n  @classmethod\n  def add(cls, x):\n    cls.xs = append(cls.xs, x)\n_ = A.add(1)\na = A()\nprint a.xs\nprint A.xs\n" `shouldBe` Right ["[1]", "[1]"]

  it "keeps class attribute assignment after instance creation" $ do
    runSourceVm "class A:\n  x = 1\nA.x = 2\na = A()\nprint A.x\nprint a.x\n" `shouldBe` Right ["2", "2"]

  it "updates instance fallback after class attribute reassignment" $ do
    runSourceVm "class A:\n  x = 1\na = A()\nA.x = 2\nprint a.x\nprint A.x\n" `shouldBe` Right ["2", "2"]

  it "keeps class attribute reassignment visible through classmethod after instance creation" $ do
    runSourceVm "class A:\n  x = 1\n  @classmethod\n  def set(cls, v):\n    cls.x = v\na = A()\n_ = A.set(2)\nprint a.x\nprint A.x\n" `shouldBe` Right ["2", "2"]

  it "supports class attribute referencing another class attribute" $ do
    runSourceVm "class A:\n  x = 1\n  y = x + 1\nprint A.y\n" `shouldBe` Right ["2"]

  it "supports class attribute referencing global variable" $ do
    runSourceVm "n = 1\nclass A:\n  x = n + 1\nprint A.x\n" `shouldBe` Right ["2"]

  it "does not overwrite instance shadowing when class attribute changes" $ do
    runSourceVm "class A:\n  x = 1\na = A()\na.x = 2\nA.x = 3\nprint a.x\nb = A()\nprint b.x\nprint A.x\n" `shouldBe` Right ["2", "3", "3"]

  it "prefers subclass method over inherited class attribute" $ do
    runSourceVm "class A:\n  x = 1\nclass B(A):\n  def x(self):\n    return 2\nb = B()\nprint b.x()\n" `shouldBe` Right ["2"]

  it "prefers subclass class attribute over inherited method" $ do
    runSourceVm "class A:\n  def x(self):\n    return 1\nclass B(A):\n  x = 2\nb = B()\nprint b.x\n" `shouldBe` Right ["2"]

  it "supports augmented assignment on class attribute" $ do
    runSourceVm "class A:\n  x = 1\nA.x += 2\nprint A.x\n" `shouldBe` Right ["3"]

  it "supports augmented assignment on instance attribute fallback" $ do
    runSourceVm "class A:\n  x = 1\na = A()\na.x += 2\nprint a.x\nprint A.x\n" `shouldBe` Right ["3", "1"]

  it "supports augmented assignment on class attribute through classmethod" $ do
    runSourceVm "class A:\n  x = 1\n  @classmethod\n  def inc(cls):\n    cls.x += 2\n_ = A.inc()\nprint A.x\na = A()\nprint a.x\n" `shouldBe` Right ["3", "3"]

  it "supports deleting class attribute" $ do
    runSourceVm "class A:\n  x = 1\ndel A.x\nprint A.x\n" `shouldBe` Left "Attribute error: undefined attribute A.x at 4:7"
