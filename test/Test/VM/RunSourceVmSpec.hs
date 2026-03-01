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
