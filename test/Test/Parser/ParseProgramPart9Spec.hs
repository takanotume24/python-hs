{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart9Spec (spec) where

import Data.Either (isRight)
import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.AST.ImportEntry (ImportEntry (..))
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.AST.MappingPatternEntry (MappingPatternEntry (..))
import PythonHS.AST.MatchCase (MatchCase (..))
import PythonHS.AST.Pattern (Pattern (AsPattern, CapturePattern, MappingPattern, OrPattern, SequencePattern, ValuePattern, WildcardPattern))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ClassDefStmt, ContinueStmt, DecoratedStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FromImportStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, MatchStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, RaiseStmt, ReturnStmt, SubAssignStmt, TryExceptStmt, WhileStmt, WithStmt, YieldFromStmt, YieldStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "parseProgram - part 9" $ do
  it "parses function definition with default parameter" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 14),
        Token IntegerToken "2" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token ColonToken ":" (Position 1 18),
        Token PrintToken "print" (Position 1 20),
        Token IdentifierToken "a" (Position 1 26),
        Token NewlineToken "\\n" (Position 1 27),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefDefaultsStmt
                "add"
                ["a", "b"]
                [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 1 16)}]
                [PrintStmt (IdentifierExpr "a" (Position 1 26)) (Position 1 20)]
                (Position 1 1)
            ]
        )

  it "parses function definition with default parameter and trailing comma" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 14),
        Token IntegerToken "2" (Position 1 16),
        Token CommaToken "," (Position 1 17),
        Token RParenToken ")" (Position 1 18),
        Token ColonToken ":" (Position 1 19),
        Token PrintToken "print" (Position 1 21),
        Token IdentifierToken "a" (Position 1 27),
        Token NewlineToken "\\n" (Position 1 28),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefDefaultsStmt
                "add"
                ["a", "b"]
                [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 1 16)}]
                [PrintStmt (IdentifierExpr "a" (Position 1 27)) (Position 1 21)]
                (Position 1 1)
            ]
        )

  it "parses keyword argument call" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token RParenToken ")" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "f" [KeywordArgExpr "a" (IntegerExpr 1 (Position 1 11)) (Position 1 9)] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses mixed positional then keyword argument call" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IntegerToken "1" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 13),
        Token IntegerToken "2" (Position 1 14),
        Token RParenToken ")" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "f"
                    [IntegerExpr 1 (Position 1 9), KeywordArgExpr "b" (IntegerExpr 2 (Position 1 14)) (Position 1 12)]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses star expansion in call arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token StarToken "*" (Position 1 9),
        Token IdentifierToken "xs" (Position 1 10),
        Token RParenToken ")" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "f" [StarArgExpr (IdentifierExpr "xs" (Position 1 10)) (Position 1 9)] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses double-star expansion in call arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token StarToken "*" (Position 1 9),
        Token StarToken "*" (Position 1 10),
        Token IdentifierToken "kw" (Position 1 11),
        Token RParenToken ")" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "f" [KwStarArgExpr (IdentifierExpr "kw" (Position 1 11)) (Position 1 9)] (Position 1 7))
                (Position 1 1)
            ]
        )
