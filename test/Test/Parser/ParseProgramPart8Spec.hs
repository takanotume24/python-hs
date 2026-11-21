{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart8Spec (spec) where

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
spec = describe "parseProgram - part 8" $ do
  it "parses nested conditional inside while suite and continues" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 7),
        Token ColonToken ":" (Position 1 11),
        Token NewlineToken "\\n" (Position 1 12),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token IfToken "if" (Position 2 3),
        Token IdentifierToken "inner" (Position 2 6),
        Token ColonToken ":" (Position 2 11),
        Token NewlineToken "\\n" (Position 2 12),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PrintToken "print" (Position 3 5),
        Token IntegerToken "1" (Position 3 11),
        Token NewlineToken "\\n" (Position 3 12),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token ElseToken "else" (Position 4 3),
        Token ColonToken ":" (Position 4 7),
        Token NewlineToken "\\n" (Position 4 8),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PrintToken "print" (Position 5 5),
        Token IntegerToken "2" (Position 5 11),
        Token NewlineToken "\\n" (Position 5 12),
        Token DedentToken "<DEDENT>" (Position 6 1),
        Token PassToken "pass" (Position 6 3),
        Token NewlineToken "\\n" (Position 6 7),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 1),
        Token IntegerToken "9" (Position 7 7),
        Token NewlineToken "\\n" (Position 7 8),
        Token EOFToken "" (Position 8 1)
      ]
      `shouldBe` Right
        ( Program
            [ WhileStmt
                (IdentifierExpr "cond" (Position 1 7))
                [ IfStmt
                    (IdentifierExpr "inner" (Position 2 6))
                    [PrintStmt (IntegerExpr 1 (Position 3 11)) (Position 3 5)]
                    (Just [PrintStmt (IntegerExpr 2 (Position 5 11)) (Position 5 5)])
                    (Position 2 3),
                  PassStmt (Position 6 3)
                ]
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 7 7)) (Position 7 1)
            ]
        )

  it "parses binary subtraction with precedence" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "5" (Position 1 7),
        Token MinusToken "-" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token StarToken "*" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( BinaryExpr
                    SubtractOperator
                    (IntegerExpr 5 (Position 1 7))
                    (BinaryExpr MultiplyOperator (IntegerExpr 2 (Position 1 11)) (IntegerExpr 3 (Position 1 15)) (Position 1 13))
                    (Position 1 9)
                )
                (Position 1 1)
            ]
        )

  it "parses floor-division operator" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "7" (Position 1 7),
        Token DoubleSlashToken "//" (Position 1 9),
        Token IntegerToken "2" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr FloorDivideOperator (IntegerExpr 7 (Position 1 7)) (IntegerExpr 2 (Position 1 12)) (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses a global statement" $ do
    parseProgram
      [ Token GlobalToken "global" (Position 1 1),
        Token IdentifierToken "x" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [GlobalStmt "x" (Position 1 1)])

  it "parses a pass statement" $ do
    parseProgram
      [ Token PassToken "pass" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 5),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PassStmt (Position 1 1)])

  it "parses function definition and call" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefStmt "add" ["a", "b"] [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Position 1 1),
              PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 1 1), IntegerExpr 2 (Position 1 1)] (Position 1 1)) (Position 1 1)
            ]
        )

  it "parses function definition and call with trailing commas" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefStmt "add" ["a", "b"] [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Position 1 1),
              PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 1 1), IntegerExpr 2 (Position 1 1)] (Position 1 1)) (Position 1 1)
            ]
        )
