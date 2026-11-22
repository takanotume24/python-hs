module Test.CPython.ParseAllowlistEntrySpec (spec) where

import PythonHS.CPython.AllowlistEntry (AllowlistEntry (..))
import PythonHS.CPython.ParseAllowlistEntry (parseAllowlistEntry)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parseAllowlistEntry" $ do
  it "parses a file-only entry" $ do
    parseAllowlistEntry "third_party/cpython/Lib/test/test_unary.py"
      `shouldBe` Right
        AllowlistEntry
          { allowlistEntryFile = "third_party/cpython/Lib/test/test_unary.py",
            allowlistEntryClassName = Nothing,
            allowlistEntryMethodName = Nothing
          }

  it "parses a class entry" $ do
    parseAllowlistEntry "test_unary.py::UnaryOpTestCase"
      `shouldBe` Right
        AllowlistEntry
          { allowlistEntryFile = "test_unary.py",
            allowlistEntryClassName = Just "UnaryOpTestCase",
            allowlistEntryMethodName = Nothing
          }

  it "parses a method entry" $ do
    parseAllowlistEntry "test_unary.py::UnaryOpTestCase::test_invert"
      `shouldBe` Right
        AllowlistEntry
          { allowlistEntryFile = "test_unary.py",
            allowlistEntryClassName = Just "UnaryOpTestCase",
            allowlistEntryMethodName = Just "test_invert"
          }

  it "trims surrounding whitespace" $ do
    parseAllowlistEntry "  test.py::T  "
      `shouldBe` Right
        AllowlistEntry
          { allowlistEntryFile = "test.py",
            allowlistEntryClassName = Just "T",
            allowlistEntryMethodName = Nothing
          }

  it "rejects too many parts" $ do
    parseAllowlistEntry "test.py::T::test_ok::extra"
      `shouldBe` Left "Invalid allowlist entry: test.py::T::test_ok::extra"

  it "rejects empty parts" $ do
    parseAllowlistEntry "test.py::"
      `shouldBe` Left "Invalid allowlist entry: test.py::"
