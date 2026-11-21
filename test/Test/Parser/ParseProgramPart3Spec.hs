{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart3Spec (spec) where

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
spec = describe "parseProgram - part 3" $ do
  it "parses try statement with multiple except suites" $ do
    parseProgram
      [ Token TryToken "try" (Position 1 1),
        Token ColonToken ":" (Position 1 4),
        Token NewlineToken "\\n" (Position 1 5),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token RaiseToken "raise" (Position 2 3),
        Token StringToken "x" (Position 2 9),
        Token NewlineToken "\\n" (Position 2 12),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ExceptToken "except" (Position 3 1),
        Token ColonToken ":" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PrintToken "print" (Position 4 3),
        Token IntegerToken "1" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token ExceptToken "except" (Position 5 1),
        Token ColonToken ":" (Position 5 7),
        Token NewlineToken "\\n" (Position 5 8),
        Token IndentToken "<INDENT>" (Position 6 1),
        Token PrintToken "print" (Position 6 3),
        Token IntegerToken "2" (Position 6 9),
        Token NewlineToken "\\n" (Position 6 10),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token EOFToken "" (Position 7 1)
      ]
      `shouldBe` Right
        ( Program
            [ TryExceptStmt
                [RaiseStmt (Just (StringExpr "x" (Position 2 9))) (Position 2 3)]
                [ ExceptSuite {exceptSuiteType = Nothing, exceptSuiteAlias = Nothing, exceptSuiteBody = [PrintStmt (IntegerExpr 1 (Position 4 9)) (Position 4 3)], exceptSuitePos = Position 3 1},
                  ExceptSuite {exceptSuiteType = Nothing, exceptSuiteAlias = Nothing, exceptSuiteBody = [PrintStmt (IntegerExpr 2 (Position 6 9)) (Position 6 3)], exceptSuitePos = Position 5 1}
                ]
                Nothing
                Nothing
                (Position 1 1)
            ]
        )

  it "parses typed except with alias" $ do
    parseProgram
      [ Token TryToken "try" (Position 1 1),
        Token ColonToken ":" (Position 1 4),
        Token NewlineToken "\\n" (Position 1 5),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token RaiseToken "raise" (Position 2 3),
        Token StringToken "x" (Position 2 9),
        Token NewlineToken "\\n" (Position 2 12),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ExceptToken "except" (Position 3 1),
        Token IdentifierToken "RuntimeError" (Position 3 8),
        Token AsToken "as" (Position 3 21),
        Token IdentifierToken "e" (Position 3 24),
        Token ColonToken ":" (Position 3 25),
        Token NewlineToken "\\n" (Position 3 26),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PrintToken "print" (Position 4 3),
        Token IdentifierToken "e" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token EOFToken "" (Position 5 1)
      ]
      `shouldBe` Right
        ( Program
            [ TryExceptStmt
                [RaiseStmt (Just (StringExpr "x" (Position 2 9))) (Position 2 3)]
                [ExceptSuite {exceptSuiteType = Just ["RuntimeError"], exceptSuiteAlias = Just "e", exceptSuiteBody = [PrintStmt (IdentifierExpr "e" (Position 4 9)) (Position 4 3)], exceptSuitePos = Position 3 1}]
                Nothing
                Nothing
                (Position 1 1)
            ]
        )

  it "parses parenthesized except types with alias" $ do
    parseProgram
      [ Token TryToken "try" (Position 1 1),
        Token ColonToken ":" (Position 1 4),
        Token NewlineToken "\\n" (Position 1 5),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token RaiseToken "raise" (Position 2 3),
        Token StringToken "x" (Position 2 9),
        Token NewlineToken "\\n" (Position 2 12),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ExceptToken "except" (Position 3 1),
        Token LParenToken "(" (Position 3 8),
        Token IdentifierToken "ValueError" (Position 3 9),
        Token CommaToken "," (Position 3 19),
        Token IdentifierToken "RuntimeError" (Position 3 21),
        Token RParenToken ")" (Position 3 33),
        Token AsToken "as" (Position 3 35),
        Token IdentifierToken "e" (Position 3 38),
        Token ColonToken ":" (Position 3 39),
        Token NewlineToken "\\n" (Position 3 40),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PrintToken "print" (Position 4 3),
        Token IdentifierToken "e" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token EOFToken "" (Position 5 1)
      ]
      `shouldBe` Right
        ( Program
            [ TryExceptStmt
                [RaiseStmt (Just (StringExpr "x" (Position 2 9))) (Position 2 3)]
                [ExceptSuite {exceptSuiteType = Just ["ValueError", "RuntimeError"], exceptSuiteAlias = Just "e", exceptSuiteBody = [PrintStmt (IdentifierExpr "e" (Position 4 9)) (Position 4 3)], exceptSuitePos = Position 3 1}]
                Nothing
                Nothing
                (Position 1 1)
            ]
        )

  it "parses else suite when try body has no error" $ do
    parseProgram
      [ Token TryToken "try" (Position 1 1),
        Token ColonToken ":" (Position 1 4),
        Token NewlineToken "\\n" (Position 1 5),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PrintToken "print" (Position 2 3),
        Token IntegerToken "1" (Position 2 9),
        Token NewlineToken "\\n" (Position 2 10),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ExceptToken "except" (Position 3 1),
        Token IdentifierToken "TypeError" (Position 3 8),
        Token ColonToken ":" (Position 3 18),
        Token NewlineToken "\\n" (Position 3 19),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PrintToken "print" (Position 4 3),
        Token IntegerToken "2" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token ElseToken "else" (Position 5 1),
        Token ColonToken ":" (Position 5 5),
        Token NewlineToken "\\n" (Position 5 6),
        Token IndentToken "<INDENT>" (Position 6 1),
        Token PrintToken "print" (Position 6 3),
        Token IntegerToken "3" (Position 6 9),
        Token NewlineToken "\\n" (Position 6 10),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token EOFToken "" (Position 7 1)
      ]
      `shouldBe` Right
        ( Program
            [ TryExceptStmt
                [PrintStmt (IntegerExpr 1 (Position 2 9)) (Position 2 3)]
                [ExceptSuite {exceptSuiteType = Just ["TypeError"], exceptSuiteAlias = Nothing, exceptSuiteBody = [PrintStmt (IntegerExpr 2 (Position 4 9)) (Position 4 3)], exceptSuitePos = Position 3 1}]
                (Just [PrintStmt (IntegerExpr 3 (Position 6 9)) (Position 6 3)])
                Nothing
                (Position 1 1)
            ]
        )
