module Test.Runner.RunnerCorePart3Spec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration core) - part 3" $ do
  it "evaluates list indexing" $ do
    runSource "print [10, 20, 30][1]\n" `shouldBe` Right ["20"]

  it "evaluates tuple indexing" $ do
    runSource "print (1, 2, 3)[0]\n" `shouldBe` Right ["1"]

  it "evaluates string indexing" $ do
    runSource "print \"abc\"[2]\n" `shouldBe` Right ["c"]

  it "evaluates negative indexing" $ do
    runSource "print [10, 20, 30][-1]\n" `shouldBe` Right ["30"]

  it "evaluates list slicing with start" $ do
    runSource "print [10, 20, 30][1:]\n" `shouldBe` Right ["[20, 30]"]

  it "evaluates tuple slicing with end" $ do
    runSource "print (1, 2, 3)[:2]\n" `shouldBe` Right ["(1, 2)"]

  it "evaluates string slicing with start and end" $ do
    runSource "print \"abcdef\"[1:4]\n" `shouldBe` Right ["bcd"]

  it "evaluates full slice" $ do
    runSource "print [10, 20, 30][:]\n" `shouldBe` Right ["[10, 20, 30]"]

  it "evaluates negative start slice" $ do
    runSource "print [10, 20, 30][-2:]\n" `shouldBe` Right ["[20, 30]"]

  it "evaluates walrus operator in print" $ do
    runSource "print (x := 5)\nprint x\n" `shouldBe` Right ["5", "5"]

  it "evaluates walrus operator in if condition" $ do
    runSource "if (n := 1):\n  print n\n" `shouldBe` Right ["1"]

  it "evaluates walrus operator in list comprehension condition" $ do
    runSource "print [x for x in [1, 2, 3, 4] if (y := x % 2) == 0]\n" `shouldBe` Right ["[2, 4]"]

  it "reports index out of range" $ do
    runSource "print [1, 2][5]\n" `shouldBe` Left "Value error: tuple/list index out of range at 1:13"
