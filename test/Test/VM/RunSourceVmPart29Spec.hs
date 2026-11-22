module Test.VM.RunSourceVmPart29Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 29" $ do
  describe "f-string interpolation" $ do
    it "evaluates a literal-only f-string" $ do
      runSourceVm "print f\"abc\"\n" `shouldBe` Right ["abc"]

    it "interpolates a name" $ do
      runSourceVm "name = 'world'\nprint f\"hello {name}\"\n" `shouldBe` Right ["hello world"]

    it "interpolates multiple expressions" $ do
      runSourceVm "a = 1\nb = 2\nprint f\"{a}+{b}={a+b}\"\n" `shouldBe` Right ["1+2=3"]

    it "supports adjacent interpolations" $ do
      runSourceVm "print f\"{1}{2}\"\n" `shouldBe` Right ["12"]

    it "supports single-quoted f-strings" $ do
      runSourceVm "x = 5\nprint f'x={x}'\n" `shouldBe` Right ["x=5"]

    it "uses str() conversion for values" $ do
      runSourceVm "x = [1, 2]\nprint f\"{x}\"\n" `shouldBe` Right ["[1, 2]"]

    it "interpolates an expression producing a string" $ do
      runSourceVm "print f\"{'a' + 'b'}\"\n" `shouldBe` Right ["ab"]

    it "evaluates dict indexing inside braces" $ do
      runSourceVm "d = {'a': 1}\nprint f\"{d['a']}\"\n" `shouldBe` Right ["1"]

    it "evaluates nested braces without treating them as the end" $ do
      runSourceVm "print f\"{ {'a': 1}['a'] }\"\n" `shouldBe` Right ["1"]

  describe "escaped braces" $ do
    it "renders doubled braces" $ do
      runSourceVm "print f\"{{x}}\"\n" `shouldBe` Right ["{x}"]

    it "mixes escapes and interpolations" $ do
      runSourceVm "x = 1\nprint f\"{{{x}}}\"\n" `shouldBe` Right ["{1}"]

  describe "format specifier" $ do
    it "formats an int with d" $ do
      runSourceVm "n = 7\nprint f\"{n:d}\"\n" `shouldBe` Right ["7"]

    it "formats a value with s" $ do
      runSourceVm "v = 3\nprint f\"{v:s}\"\n" `shouldBe` Right ["3"]

    it "formats a float with two decimals" $ do
      runSourceVm "x = 1.234\nprint f\"{x:.2f}\"\n" `shouldBe` Right ["1.23"]

    it "formats a float with the default f precision" $ do
      runSourceVm "x = 2.0\nprint f\"{x:f}\"\n" `shouldBe` Right ["2.000000"]

    it "reports an unsupported spec" $ do
      runSourceVm "print f\"{1:q}\"\n" `shouldBe` Left "Value error: unsupported format spec q at 1:7"

    it "reports d on a string" $ do
      runSourceVm "print f\"{'a':d}\"\n" `shouldBe` Left "Type error: d format expects int at 1:7"

  describe "parity and errors" $ do
    it "matches str() for plain interpolation" $ do
      runSourceVm "x = 12\nprint f\"{x}\" == str(x)\n" `shouldBe` Right ["1"]

    it "reports an undefined name" $ do
      runSourceVm "print f\"{missing}\"\n" `shouldBe` Left "Name error: undefined identifier missing at 1:1"

    it "reports an unterminated interpolation" $ do
      runSourceVm "print f\"{1\"\n" `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"
