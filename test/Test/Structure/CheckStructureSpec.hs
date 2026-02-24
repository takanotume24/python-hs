module Test.Structure.CheckStructureSpec (spec) where

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import PythonHS.Structure.CheckStructure (checkStructureViolations)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "checkStructureViolations" $ do
  it "passes for a compliant src tree" $
    withSystemTempDirectory "check-structure-ok" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS")
      writeFile
        (root ++ "/src/PythonHS/FooBar.hs")
        "module PythonHS.FooBar (fooBar) where\n\nfooBar :: Int\nfooBar = 1\n"
      violations <- checkStructureViolations root
      violations `shouldBe` []

  it "detects multiple top-level functions and module/file mismatch" $
    withSystemTempDirectory "check-structure-ng" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS")
      writeFile
        (root ++ "/src/PythonHS/FooBar.hs")
        "module PythonHS.Wrong (fooBar) where\n\nfooBar :: Int\nfooBar = 1\notherFn :: Int\notherFn = 2\n"
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "module name mismatch")
      violations `shouldSatisfy` any (isInfixOf "top-level function declarations=2")

  it "detects file-name mismatch for a single function file" $
    withSystemTempDirectory "check-structure-filename" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS")
      writeFile
        (root ++ "/src/PythonHS/Wrong.hs")
        "module PythonHS.Wrong (fooBar) where\n\nfooBar :: Int\nfooBar = 1\n"
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "file name mismatch")

  it "detects multiple top-level type declarations" $
    withSystemTempDirectory "check-structure-types" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS")
      writeFile
        (root ++ "/src/PythonHS/Foo.hs")
        "module PythonHS.Foo (A, B) where\n\ntype A = Int\ntype B = Int\n"
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "type/data/newtype/class declarations=2")

  it "detects non-Spec filename in test directory" $
    withSystemTempDirectory "check-structure-testname" $ \root -> do
      createDirectoryIfMissing True (root ++ "/test/Test/Foo")
      writeFile
        (root ++ "/test/Test/Foo/Bar.hs")
        "module Test.Foo.Bar (spec) where\n\nspec :: Int\nspec = 1\n"
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "test file name must end with Spec")

