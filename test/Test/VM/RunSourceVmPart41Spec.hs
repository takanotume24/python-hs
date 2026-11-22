module Test.VM.RunSourceVmPart41Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm bytes support" $ do
  describe "bytes literals" $ do
    it "prints a bytes literal with the b prefix" $ do
      runSourceVm "print b\"abc\"\n"
        `shouldBe` Right ["b'abc'"]

    it "prints an empty bytes literal" $ do
      runSourceVm "print b\"\"\n"
        `shouldBe` Right ["b''"]

    it "escapes a newline in the representation" $ do
      runSourceVm "print b\"a\\nb\"\n"
        `shouldBe` Right ["b'a\\nb'"]

    it "uses double quotes when the bytes contain a single quote" $ do
      runSourceVm "print b\"it's\"\n"
        `shouldBe` Right ["b\"it's\""]

  describe "bytes protocol" $ do
    it "reports length" $ do
      runSourceVm "print len(b\"abc\")\n"
        `shouldBe` Right ["3"]

    it "indexes a bytes value to an int" $ do
      runSourceVm "print b\"abc\"[1]\n"
        `shouldBe` Right ["98"]

    it "slices a bytes value to a bytes value" $ do
      runSourceVm "print b\"abcd\"[1:3]\n"
        `shouldBe` Right ["b'bc'"]

    it "iterates over ints" $ do
      runSourceVm "for x in b\"AB\":\n    print x\n"
        `shouldBe` Right ["65", "66"]

    it "compares bytes values" $ do
      runSourceVm "print b\"abc\" == b\"abc\"\nprint b\"abc\" == b\"abd\"\n"
        `shouldBe` Right ["1", "0"]

    it "checks membership of an int and of a subsequence" $ do
      runSourceVm "print 97 in b\"abc\"\nprint b\"bc\" in b\"abc\"\nprint 122 in b\"abc\"\n"
        `shouldBe` Right ["1", "1", "0"]

  describe "bytes operations" $ do
    it "concatenates bytes values" $ do
      runSourceVm "print b\"ab\" + b\"cd\"\n"
        `shouldBe` Right ["b'abcd'"]

    it "repeats a bytes value" $ do
      runSourceVm "print b\"ab\" * 3\n"
        `shouldBe` Right ["b'ababab'"]

    it "builds bytes from an iterable of ints" $ do
      runSourceVm "print bytes([65, 66, 67])\n"
        `shouldBe` Right ["b'ABC'"]

    it "builds bytes of zeroes from an int" $ do
      runSourceVm "print bytes(3)\n"
        `shouldBe` Right ["b'\\x00\\x00\\x00'"]

    it "builds bytes from a string and encoding" $ do
      runSourceVm "print bytes(\"abc\", \"utf-8\")\n"
        `shouldBe` Right ["b'abc'"]

  describe "bytearray" $ do
    it "prints a bytearray value" $ do
      runSourceVm "print bytearray(b\"ab\")\n"
        `shouldBe` Right ["bytearray(b'ab')"]

    it "supports length and indexing" $ do
      runSourceVm "ba = bytearray(b\"abc\")\nprint len(ba)\nprint ba[0]\n"
        `shouldBe` Right ["3", "97"]

  describe "encode and decode" $ do
    it "decodes bytes to a string" $ do
      runSourceVm "print b\"hello\".decode()\n"
        `shouldBe` Right ["hello"]

    it "decodes bytes with an explicit encoding" $ do
      runSourceVm "print b\"hello\".decode(\"utf-8\")\n"
        `shouldBe` Right ["hello"]

    it "encodes a string to bytes" $ do
      runSourceVm "print \"hello\".encode()\n"
        `shouldBe` Right ["b'hello'"]

    it "encodes a string with an explicit encoding" $ do
      runSourceVm "print \"hello\".encode(\"utf-8\")\n"
        `shouldBe` Right ["b'hello'"]

    it "round-trips a non-ascii string through utf-8" $ do
      runSourceVm "print \"あ\".encode().decode()\n"
        `shouldBe` Right ["あ"]
