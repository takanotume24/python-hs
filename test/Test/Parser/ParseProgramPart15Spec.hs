{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart15Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (GteOperator, LtOperator))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (PrintStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parseProgram - part 15" $ do
  it "parses chained comparison expression" $
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token LtToken "<" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token LtToken "<" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (ComparisonChainExpr {comparisonChainLeft = IntegerExpr 1 (Position 1 7), comparisonChainOps = [LtOperator, LtOperator], comparisonChainRights = [IntegerExpr 2 (Position 1 11), IntegerExpr 3 (Position 1 15)], comparisonChainPos = Position 1 9}) (Position 1 1)])

  it "parses chained comparison with mixed operators" $
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token LtToken "<" (Position 1 9),
        Token IntegerToken "3" (Position 1 11),
        Token GteToken ">=" (Position 1 13),
        Token IntegerToken "2" (Position 1 16),
        Token NewlineToken "\n" (Position 1 17),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (ComparisonChainExpr {comparisonChainLeft = IntegerExpr 1 (Position 1 7), comparisonChainOps = [LtOperator, GteOperator], comparisonChainRights = [IntegerExpr 3 (Position 1 11), IntegerExpr 2 (Position 1 16)], comparisonChainPos = Position 1 9}) (Position 1 1)])
