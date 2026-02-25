module Test.Structure.CheckStructureSpec (spec) where

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import PythonHS.Structure.CheckStructure (checkStructureViolations, checkStructureWarnings)
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

  it "detects files exceeding 200 lines" $
    withSystemTempDirectory "check-structure-lines" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS")
      let bodyLines = replicate 198 ""
          content = unlines (["module PythonHS.LongFile (longFile) where", "", "longFile :: Int"] ++ bodyLines)
      writeFile
        (root ++ "/src/PythonHS/LongFile.hs")
        content
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "file exceeds 200 lines")

  it "does not apply line-limit checks to test files" $
    withSystemTempDirectory "check-structure-test-lines" $ \root -> do
      createDirectoryIfMissing True (root ++ "/test/Test/Parser")
      let bodyLines = replicate 250 ""
          content = unlines (["module Test.Parser.ParseProgramSpec (spec) where", "", "spec :: Int", "spec = 1"] ++ bodyLines)
      writeFile
        (root ++ "/test/Test/Parser/ParseProgramSpec.hs")
        content
      violations <- checkStructureViolations root
      violations `shouldBe` []

  it "does not skip line-limit checks for legacy files anymore" $
    withSystemTempDirectory "check-structure-lines-exempt" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS/Evaluator")
      let bodyLines = replicate 198 ""
          content = unlines (["module PythonHS.Evaluator.EvalStatements (evalStatements) where", "", "evalStatements :: Int", "evalStatements = 1"] ++ bodyLines)
      writeFile
        (root ++ "/src/PythonHS/Evaluator/EvalStatements.hs")
        content
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "file exceeds 200 lines")

  it "does not exempt ParseProgram from line-limit checks" $
    withSystemTempDirectory "check-structure-lines-parse-program" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS/Parser")
      let bodyLines = replicate 198 ""
          content = unlines (["module PythonHS.Parser.ParseProgram (parseProgram) where", "", "parseProgram :: Int", "parseProgram = 1"] ++ bodyLines)
      writeFile
        (root ++ "/src/PythonHS/Parser/ParseProgram.hs")
        content
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "file exceeds 200 lines")

  it "does not exempt ScanTokens from line-limit checks" $
    withSystemTempDirectory "check-structure-lines-scan-tokens" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS/Lexer")
      let bodyLines = replicate 198 ""
          content = unlines (["module PythonHS.Lexer.ScanTokens (scanTokens) where", "", "scanTokens :: Int", "scanTokens = 1"] ++ bodyLines)
      writeFile
        (root ++ "/src/PythonHS/Lexer/ScanTokens.hs")
        content
      violations <- checkStructureViolations root
      violations `shouldSatisfy` any (isInfixOf "file exceeds 200 lines")

  it "does not report temporary line-limit warnings when exemptions are empty" $
    withSystemTempDirectory "check-structure-warnings" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS/Evaluator")
      writeFile
        (root ++ "/src/PythonHS/Evaluator/EvalStatements.hs")
        "module PythonHS.Evaluator.EvalStatements (evalStatements) where\n\nevalStatements :: Int\nevalStatements = 1\n"
      warnings <- checkStructureWarnings root
      warnings `shouldBe` []

  it "does not report warning when exempt file is absent" $
    withSystemTempDirectory "check-structure-no-warnings" $ \root -> do
      warnings <- checkStructureWarnings root
      warnings `shouldBe` []

  it "returns empty warnings when no temporary exemptions exist" $
    withSystemTempDirectory "check-structure-warning-order" $ \root -> do
      createDirectoryIfMissing True (root ++ "/src/PythonHS/Evaluator")
      writeFile
        (root ++ "/src/PythonHS/Evaluator/EvalStatements.hs")
        (unlines (["module PythonHS.Evaluator.EvalStatements (evalStatements) where", "", "evalStatements :: Int", "evalStatements = 1"] ++ replicate 10 ""))
      warnings <- checkStructureWarnings root
      warnings `shouldBe` []

