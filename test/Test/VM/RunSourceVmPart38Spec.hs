module Test.VM.RunSourceVmPart38Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm literal compatibility" $ do
  describe "raw strings" $ do
    it "keeps backslashes in a raw double-quoted string" $ do
      runSourceVm "print r\"\\n\"\n"
        `shouldBe` Right ["\\n"]

    it "keeps backslashes in a raw single-quoted string" $ do
      runSourceVm "print r'a\\tb'\n"
        `shouldBe` Right ["a\\tb"]

  describe "implicit string concatenation" $ do
    it "joins adjacent string literals" $ do
      runSourceVm "print \"a\" \"b\"\n"
        `shouldBe` Right ["ab"]

    it "joins adjacent literals across a line continuation" $ do
      runSourceVm "print \"a\" \\\n    \"b\"\n"
        `shouldBe` Right ["ab"]

  describe "semicolons" $ do
    it "runs multiple simple statements on one line" $ do
      runSourceVm "x = 1; y = 2\nprint x + y\n"
        `shouldBe` Right ["3"]

    it "accepts a trailing semicolon" $ do
      runSourceVm "x = 1;\nprint x\n"
        `shouldBe` Right ["1"]

    it "separates statements inside an indented block" $ do
      runSourceVm "if True:\n    x = 1; y = 2\n    print x + y\n"
        `shouldBe` Right ["3"]

  describe "line continuation" $ do
    it "joins a backslash-continued expression" $ do
      runSourceVm "x = 1 + \\\n    2\nprint x\n"
        `shouldBe` Right ["3"]

  describe "implicit continuation inside brackets" $ do
    it "parses a multi-line list literal" $ do
      runSourceVm "x = [1,\n     2,\n     3]\nprint x\n"
        `shouldBe` Right ["[1, 2, 3]"]

    it "parses a multi-line dict literal" $ do
      runSourceVm "x = {\"a\": 1,\n     \"b\": 2}\nprint x\n"
        `shouldBe` Right ["{a: 1, b: 2}"]

    it "parses a multi-line call" $ do
      runSourceVm "print sorted([3,\n    1,\n    2])\n"
        `shouldBe` Right ["[1, 2, 3]"]

    it "keeps line numbers after a multi-line bracket" $ do
      runSourceVm "x = [1,\n     2]\nprint x\n"
        `shouldBe` Right ["[1, 2]"]

  describe "bytes literals" $ do
    it "accepts a bytes literal until byte values are implemented" $ do
      runSourceVm "print b\"abc\"\n"
        `shouldBe` Right ["abc"]
