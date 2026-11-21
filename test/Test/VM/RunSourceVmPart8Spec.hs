module Test.VM.RunSourceVmPart8Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 8" $ do
  describe "str builtin" $ do
    it "converts int to string" $ do
      runSourceVm "print str(12)\n" `shouldBe` Right ["12"]

    it "converts float to string" $ do
      runSourceVm "print str(1.5)\n" `shouldBe` Right ["1.5"]

    it "keeps string unchanged" $ do
      runSourceVm "print str('abc')\n" `shouldBe` Right ["abc"]

    it "renders list as textual form" $ do
      runSourceVm "print str([1, 2])\n" `shouldBe` Right ["[1, 2]"]

    it "renders None as textual form" $ do
      runSourceVm "print str(None)\n" `shouldBe` Right ["None"]

    it "reports argument count mismatch" $ do
      runSourceVm "print str()\n" `shouldBe` Left "Argument count mismatch when calling str at 1:7"

  describe "int builtin" $ do
    it "keeps int unchanged" $ do
      runSourceVm "print int(12)\n" `shouldBe` Right ["12"]

    it "truncates float toward zero" $ do
      runSourceVm "print int(1.9)\nprint int(-1.9)\n" `shouldBe` Right ["1", "-1"]

    it "parses integer string" $ do
      runSourceVm "print int('12')\nprint int('-7')\n" `shouldBe` Right ["12", "-7"]

    it "ignores surrounding whitespace in integer string" $ do
      runSourceVm "print int(' 12 ')\n" `shouldBe` Right ["12"]

    it "reports value error for non-integer string" $ do
      runSourceVm "print int('1.5')\n" `shouldBe` Left "Value error: invalid literal for int at 1:7"

    it "reports type error for unsupported value" $ do
      runSourceVm "print int([1])\n" `shouldBe` Left "Type error: int expects int, float, or string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print int()\n" `shouldBe` Left "Argument count mismatch when calling int at 1:7"

  describe "float builtin" $ do
    it "converts int to float" $ do
      runSourceVm "print float(2)\n" `shouldBe` Right ["2.0"]

    it "keeps float unchanged" $ do
      runSourceVm "print float(1.5)\n" `shouldBe` Right ["1.5"]

    it "parses float string" $ do
      runSourceVm "print float('1.5')\nprint float('-2.5')\n" `shouldBe` Right ["1.5", "-2.5"]

    it "parses exponential notation" $ do
      runSourceVm "print float('1e3')\n" `shouldBe` Right ["1000.0"]

    it "reports value error for non-numeric string" $ do
      runSourceVm "print float('abc')\n" `shouldBe` Left "Value error: could not convert string to float at 1:7"

    it "reports type error for unsupported value" $ do
      runSourceVm "print float([1])\n" `shouldBe` Left "Type error: float expects int, float, or string at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print float()\n" `shouldBe` Left "Argument count mismatch when calling float at 1:7"

  describe "conversion composition" $ do
    it "composes int and float conversion" $ do
      runSourceVm "print int(float('3.9'))\n" `shouldBe` Right ["3"]

    it "composes str and int conversion" $ do
      runSourceVm "print str(int(7.9))\n" `shouldBe` Right ["7"]
