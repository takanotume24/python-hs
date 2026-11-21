module Test.VM.RunSourceVmPart17Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 17" $ do
  describe "finally and raised exceptions" $ do
    it "propagates a raised error after running finally" $ do
      runSourceVm "try:\n  raise \"boom\"\nfinally:\n  print 1\n" `shouldBe` Left "Runtime error: boom at 2:3"

    it "propagates a runtime error after running finally" $ do
      runSourceVm "try:\n  print 1 / 0\nfinally:\n  print 2\n" `shouldBe` Left "Value error: division by zero at 2:11"

    it "runs finally before propagating an error raised in else" $ do
      runSourceVm "try:\n  print 1\nexcept:\n  print 2\nelse:\n  raise 'late'\nfinally:\n  print 3\n" `shouldBe` Left "Runtime error: late at 6:3"

    it "runs finally when an inner finally propagates to an outer except" $ do
      runSourceVm "def f():\n  try:\n    try:\n      raise 'x'\n    finally:\n      print 1\n  except:\n    print 2\n  return 3\nprint f()\n" `shouldBe` Right ["1", "2", "3"]

    it "runs finally on bare re-raise" $ do
      runSourceVm "try:\n  raise ValueError('boom')\nexcept ValueError:\n  try:\n    raise\n  finally:\n    print 1\n" `shouldBe` Left "Value error: boom at 2:3"

    it "replaces the propagating error with one raised in finally" $ do
      runSourceVm "try:\n  raise 'first'\nfinally:\n  raise 'second'\n" `shouldBe` Left "Runtime error: second at 4:3"

  describe "bare raise without an active exception" $ do
    it "reports no active exception at module level" $ do
      runSourceVm "raise\n" `shouldBe` Left "Runtime error: No active exception to re-raise"

    it "reports no active exception inside a function" $ do
      runSourceVm "def f():\n  raise\nf()\n" `shouldBe` Left "Runtime error: No active exception to re-raise"

  describe "raise with a builtin exception class" $ do
    it "raises ValueError without calling it" $ do
      runSourceVm "try:\n  raise ValueError\nexcept ValueError:\n  print 1\n" `shouldBe` Right ["1"]

    it "raises TypeError without calling it" $ do
      runSourceVm "try:\n  raise TypeError\nexcept TypeError:\n  print 1\n" `shouldBe` Right ["1"]

    it "propagates an unmatched bare exception class" $ do
      runSourceVm "try:\n  raise ValueError\nexcept TypeError:\n  print 1\n" `shouldBe` Left "Value error:  at 2:3"
