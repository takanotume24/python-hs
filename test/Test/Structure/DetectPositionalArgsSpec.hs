module Test.Structure.DetectPositionalArgsSpec (spec) where

import Data.Aeson (decode)
import Data.Aeson.Types (Object)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.Char8 as BSL
import PythonHS.Structure.DetectPositionalArgs (detectPositionalArgsFromSource)
import PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..))
import PythonHS.Structure.FormatViolationsJson (formatViolationsJson)
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation (..))
import PythonHS.Structure.ViolationCategory (ViolationCategory (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "detectPositionalArgs" $ do
  it "detects non-record data declarations" $ do
    let src = unlines ["module Foo where", "data Foo = Foo Int String"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Foo.hs" src)
    length result `shouldSatisfy` (>= 1)
    case result of
      (v : _) -> do
        filePath v `shouldBe` "Foo.hs"
        category v `shouldBe` DataConCategory
      _ -> return ()

  it "detects non-record newtype declarations" $ do
    let src = unlines ["module Bar where", "newtype Bar = Bar Int"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Bar.hs" src)
    length result `shouldSatisfy` (>= 1)
    case result of
      (v : _) -> category v `shouldBe` DataConCategory
      _ -> return ()

  it "does not detect record data declarations" $ do
    let src = unlines ["module Baz where", "data Baz = Baz { field :: Int }"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Baz.hs" src)
    result `shouldSatisfy` null

  it "does not detect nullary constructors" $ do
    let src = unlines ["module Foo where", "data Foo = A | B | C"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Foo.hs" src)
    result `shouldSatisfy` null

  it "detects function declarations with 2+ positional arguments" $ do
    let src = unlines ["module Quux where", "f a b = a + b"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Quux.hs" src)
    result `shouldSatisfy` any (\v -> category v == FunDeclCategory)

  it "does not detect functions with 0 or 1 argument" $ do
    let src = unlines ["module X where", "f = 1", "g x = x"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "X.hs" src)
    result `shouldSatisfy` null

  it "detects tuple expressions with 2+ elements" $ do
    let src = unlines ["module Y where", "f = (1, 2)"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Y.hs" src)
    length result `shouldSatisfy` (>= 1)
    result `shouldSatisfy` any (\v -> category v == TupleCategory)

  it "does not detect builtin constructors like Just and Left" $ do
    let src = unlines ["module Foo where", "f = Just 1", "g = Left \"x\""]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Foo.hs" src)
    result `shouldSatisfy` null

  it "produces valid JSON output" $ do
    let src = unlines ["module Z where", "data Z = Z Int"]
    result <- detectPositionalArgsFromSource (DetectSourceConfig "Z.hs" src)
    let json = formatViolationsJson result
    (decode (BSL.pack json) :: Maybe [Object]) `shouldSatisfy` isJust
