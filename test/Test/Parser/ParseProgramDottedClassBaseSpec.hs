module Test.Parser.ParseProgramDottedClassBaseSpec (spec) where

import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parseProgram dotted class bases" $ do
  it "parses a dotted base class name" $ do
    case scanTokens "class T(unittest.TestCase):\n  pass\n" of
      Left err -> fail (show err)
      Right tokens ->
        parseProgram tokens
          `shouldBe` Right
            ( Program
                [ ClassDefStmt
                    { classDefStmtName = "T",
                      classDefStmtBase = Just "unittest.TestCase",
                      classDefStmtBody = [PassStmt {passStmtPos = Position 2 3}],
                      classDefStmtPos = Position 1 1
                    }
                ]
            )
