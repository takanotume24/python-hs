{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart10Spec (spec) where

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
spec = describe "parseProgram - part 10" $ do
  it "parses positional then multiple keyword arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IntegerToken "1" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 13),
        Token IntegerToken "2" (Position 1 14),
        Token CommaToken "," (Position 1 15),
        Token IdentifierToken "c" (Position 1 17),
        Token AssignToken "=" (Position 1 18),
        Token IntegerToken "3" (Position 1 19),
        Token RParenToken ")" (Position 1 20),
        Token NewlineToken "\\n" (Position 1 21),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "f"
                    [ IntegerExpr 1 (Position 1 9),
                      KeywordArgExpr "b" (IntegerExpr 2 (Position 1 14)) (Position 1 12),
                      KeywordArgExpr "c" (IntegerExpr 3 (Position 1 19)) (Position 1 17)
                    ]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses multiple keyword arguments with trailing comma" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IdentifierToken "b" (Position 1 14),
        Token AssignToken "=" (Position 1 15),
        Token IntegerToken "2" (Position 1 16),
        Token CommaToken "," (Position 1 17),
        Token RParenToken ")" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "f"
                    [ KeywordArgExpr "a" (IntegerExpr 1 (Position 1 11)) (Position 1 9),
                      KeywordArgExpr "b" (IntegerExpr 2 (Position 1 16)) (Position 1 14)
                    ]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses method-call syntax as function-style call" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "append" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IntegerToken "3" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token NewlineToken "\\n" (Position 1 18),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "append" [IdentifierExpr "x" (Position 1 7), IntegerExpr 3 (Position 1 16)] (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses chained method-call syntax" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "append" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IntegerToken "3" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token DotToken "." (Position 1 18),
        Token IdentifierToken "append" (Position 1 19),
        Token LParenToken "(" (Position 1 25),
        Token IntegerToken "4" (Position 1 26),
        Token RParenToken ")" (Position 1 27),
        Token NewlineToken "\\n" (Position 1 28),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "append"
                    [ CallExpr "append" [IdentifierExpr "x" (Position 1 7), IntegerExpr 3 (Position 1 16)] (Position 1 9),
                      IntegerExpr 4 (Position 1 26)
                    ]
                    (Position 1 19)
                )
                (Position 1 1)
            ]
        )

  it "parses method-call syntax with multiple arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "d" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "update" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IntegerToken "1" (Position 1 16),
        Token CommaToken "," (Position 1 17),
        Token IntegerToken "9" (Position 1 19),
        Token RParenToken ")" (Position 1 20),
        Token NewlineToken "\\n" (Position 1 21),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "update" [IdentifierExpr "d" (Position 1 7), IntegerExpr 1 (Position 1 16), IntegerExpr 9 (Position 1 19)] (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses method-call syntax with keyword argument" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "d" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "update" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IdentifierToken "k" (Position 1 16),
        Token AssignToken "=" (Position 1 17),
        Token IntegerToken "1" (Position 1 18),
        Token RParenToken ")" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "update" [IdentifierExpr "d" (Position 1 7), KeywordArgExpr "k" (IntegerExpr 1 (Position 1 18)) (Position 1 16)] (Position 1 9))
                (Position 1 1)
            ]
        )
