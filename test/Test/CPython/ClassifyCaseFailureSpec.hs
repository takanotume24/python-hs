module Test.CPython.ClassifyCaseFailureSpec (spec) where

import PythonHS.CPython.CaseFailureCategory (CaseFailureCategory (..))
import PythonHS.CPython.ClassifyCaseFailure (classifyCaseFailure)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "classifyCaseFailure" $ do
  it "classifies import errors" $ do
    classifyCaseFailure "Import error: module not found unittest"
      `shouldBe` ImportFailure

  it "classifies parse errors" $ do
    classifyCaseFailure "ExpectedExpression (Position {line = 1, column = 1})"
      `shouldBe` ParseFailure

  it "classifies assertion failures" $ do
    classifyCaseFailure "AssertionError: 1 != 2 at 4:14"
      `shouldBe` AssertionFailure

  it "classifies missing success markers" $ do
    classifyCaseFailure "missing success marker, output: something"
      `shouldBe` MissingMarkerFailure

  it "classifies other runtime errors" $ do
    classifyCaseFailure "Type error: + expects int+int or string+string at 1:1"
      `shouldBe` RuntimeFailure
