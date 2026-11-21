module Test.VM.RunSourceVmPart18Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 18" $ do
  describe "return through finally" $ do
    it "runs finally before returning" $ do
      runSourceVm "def f():\n  try:\n    return 1\n  finally:\n    print 'cleanup'\nprint f()\n" `shouldBe` Right ["cleanup", "1"]

    it "lets return in finally override the earlier return" $ do
      runSourceVm "def f():\n  try:\n    return 1\n  finally:\n    return 2\nprint f()\n" `shouldBe` Right ["2"]

    it "runs nested finally blocks before returning" $ do
      runSourceVm "def f():\n  try:\n    return 1\n  finally:\n    try:\n      print 2\n    finally:\n      print 3\nprint f()\n" `shouldBe` Right ["2", "3", "1"]

    it "runs finally when returning from except" $ do
      runSourceVm "def f():\n  try:\n    raise 1\n  except:\n    return 5\n  finally:\n    print 'done'\nprint f()\n" `shouldBe` Right ["done", "5"]

    it "runs finally when returning from else" $ do
      runSourceVm "def f():\n  try:\n    x = 1\n  except:\n    return 2\n  else:\n    return 3\n  finally:\n    print 'done'\nprint f()\n" `shouldBe` Right ["done", "3"]

    it "lets an error raised in finally override the return" $ do
      runSourceVm "def f():\n  try:\n    return 1\n  finally:\n    raise 'boom'\nprint f()\n" `shouldBe` Left "Runtime error: boom at 5:5"

  describe "break through finally" $ do
    it "runs finally before leaving a for loop" $ do
      runSourceVm "for i in [1, 2]:\n  try:\n    break\n  finally:\n    print i\n" `shouldBe` Right ["1"]

    it "runs finally once for a break" $ do
      runSourceVm "for i in [1, 2]:\n  try:\n    break\n  finally:\n    print 'once'\n" `shouldBe` Right ["once"]

    it "runs finally before leaving a while loop" $ do
      runSourceVm "x = 0\nwhile x < 3:\n  try:\n    break\n  finally:\n    print x\n" `shouldBe` Right ["0"]

    it "runs only the inner finally for a break in a nested loop" $ do
      runSourceVm "for i in [1]:\n  try:\n    for j in [2]:\n      try:\n        break\n      finally:\n        print 'inner'\n  finally:\n    print 'outer'\n" `shouldBe` Right ["inner", "outer"]

    it "runs finally before the statement after the loop" $ do
      runSourceVm "try:\n  for i in [1]:\n    try:\n      break\n    finally:\n      print 'inner'\n  print 'after'\nfinally:\n  print 'outer'\n" `shouldBe` Right ["inner", "after", "outer"]

  describe "continue through finally" $ do
    it "runs finally on each continue of a for loop" $ do
      runSourceVm "for i in [1, 2]:\n  try:\n    continue\n  finally:\n    print i\nprint 9\n" `shouldBe` Right ["1", "2", "9"]

    it "runs finally on each continue of a while loop" $ do
      runSourceVm "x = 0\nwhile x < 2:\n  x = x + 1\n  try:\n    continue\n  finally:\n    print x\nprint 9\n" `shouldBe` Right ["1", "2", "9"]
