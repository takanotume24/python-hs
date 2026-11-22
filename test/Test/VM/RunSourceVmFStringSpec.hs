module Test.VM.RunSourceVmFStringSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm f-string extensions" $ do
  describe "conversion flags" $ do
    it "formats with repr conversion" $ do
      runSourceVm "x = 'abc'\nprint f\"{x!r}\"\n" `shouldBe` Right ["'abc'"]

    it "formats with str conversion" $ do
      runSourceVm "x = 'abc'\nprint f\"{x!s}\"\n" `shouldBe` Right ["abc"]

    it "applies a format spec after repr" $ do
      runSourceVm "x = 'ab'\nprint f\"{x!r:>6}\"\n" `shouldBe` Right ["  'ab'"]

    it "reports an unsupported conversion" $ do
      runSourceVm "print f\"{1!x}\"\n"
        `shouldBe` Left "ExpectedExpression (Position {line = 1, column = 7})"

  describe "numeric formatting" $ do
    it "pads an int to a width" $ do
      runSourceVm "print f\"{7:5d}\"\n" `shouldBe` Right ["    7"]

    it "left aligns an int" $ do
      runSourceVm "print f\"{7:<5d}\"\n" `shouldBe` Right ["7    "]

    it "center aligns an int" $ do
      runSourceVm "print f\"{7:^5d}\"\n" `shouldBe` Right ["  7  "]

    it "uses a fill character" $ do
      runSourceVm "print f\"{7:*<5d}\"\n" `shouldBe` Right ["7****"]

    it "zero pads an int" $ do
      runSourceVm "print f\"{7:05d}\"\n" `shouldBe` Right ["00007"]

    it "adds a plus sign" $ do
      runSourceVm "print f\"{7:+d}\"\n" `shouldBe` Right ["+7"]

    it "adds a space sign" $ do
      runSourceVm "print f\"{7: d}\"\n" `shouldBe` Right [" 7"]

    it "groups thousands" $ do
      runSourceVm "print f\"{1234567:,d}\"\n" `shouldBe` Right ["1,234,567"]

    it "pads a float" $ do
      runSourceVm "print f\"{1.234:8.2f}\"\n" `shouldBe` Right ["    1.23"]

    it "zero pads a float" $ do
      runSourceVm "print f\"{1.234:08.2f}\"\n" `shouldBe` Right ["00001.23"]

  describe "string alignment" $ do
    it "right aligns a string" $ do
      runSourceVm "s = 'ab'\nprint f\"{s:>5}\"\n" `shouldBe` Right ["   ab"]

    it "centers a string" $ do
      runSourceVm "s = 'ab'\nprint f\"{s:^5}\"\n" `shouldBe` Right [" ab  "]

    it "left aligns a string by default" $ do
      runSourceVm "s = 'ab'\nprint f\"{s:5}\"\n" `shouldBe` Right ["ab   "]

  describe "nested f-strings" $ do
    it "nests an f-string with different quotes" $ do
      runSourceVm "x = 1\nprint f\"{f'{x}'}\"\n" `shouldBe` Right ["1"]

    it "reuses the same quote inside an expression" $ do
      runSourceVm "d = {'k': 2}\nprint f\"{d[\"k\"]}\"\n" `shouldBe` Right ["2"]

    it "reuses the same quote in a nested f-string" $ do
      runSourceVm "x = 3\nprint f\"{f\"{x}\"}\"\n" `shouldBe` Right ["3"]
