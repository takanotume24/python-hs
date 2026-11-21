{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart4Spec (spec) where

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
spec = describe "parseProgram - part 4" $ do
  it "parses match statement with OR/guard/mapping/wildcard patterns" $ do
    parseProgram
      [ Token MatchToken "match" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token CaseToken "case" (Position 2 3),
        Token IntegerToken "1" (Position 2 8),
        Token PipeToken "|" (Position 2 10),
        Token IntegerToken "2" (Position 2 12),
        Token IfToken "if" (Position 2 14),
        Token IdentifierToken "ok" (Position 2 17),
        Token ColonToken ":" (Position 2 19),
        Token NewlineToken "\\n" (Position 2 20),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PrintToken "print" (Position 3 5),
        Token IntegerToken "10" (Position 3 11),
        Token NewlineToken "\\n" (Position 3 13),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token CaseToken "case" (Position 4 3),
        Token LBraceToken "{" (Position 4 8),
        Token StringToken "k" (Position 4 9),
        Token ColonToken ":" (Position 4 12),
        Token IdentifierToken "v" (Position 4 14),
        Token RBraceToken "}" (Position 4 15),
        Token ColonToken ":" (Position 4 16),
        Token NewlineToken "\\n" (Position 4 17),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PrintToken "print" (Position 5 5),
        Token IdentifierToken "v" (Position 5 11),
        Token NewlineToken "\\n" (Position 5 12),
        Token DedentToken "<DEDENT>" (Position 6 1),
        Token CaseToken "case" (Position 6 3),
        Token IdentifierToken "_" (Position 6 8),
        Token ColonToken ":" (Position 6 9),
        Token NewlineToken "\\n" (Position 6 10),
        Token IndentToken "<INDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 5),
        Token IntegerToken "0" (Position 7 11),
        Token NewlineToken "\\n" (Position 7 12),
        Token DedentToken "<DEDENT>" (Position 8 1),
        Token DedentToken "<DEDENT>" (Position 8 1),
        Token EOFToken "" (Position 8 1)
      ]
      `shouldBe` Right
        ( Program
            [ MatchStmt
                (IdentifierExpr "x" (Position 1 7))
                [ MatchCase
                    { matchCasePattern =
                        OrPattern
                          [ ValuePattern (IntegerExpr 1 (Position 2 8)) (Position 2 8),
                            ValuePattern (IntegerExpr 2 (Position 2 12)) (Position 2 12)
                          ]
                          (Position 2 10),
                      matchCaseGuard = Just (IdentifierExpr "ok" (Position 2 17)),
                      matchCaseBody = [PrintStmt (IntegerExpr 10 (Position 3 11)) (Position 3 5)],
                      matchCasePos = Position 2 3
                    },
                  MatchCase
                    { matchCasePattern =
                        MappingPattern
                          [MappingPatternEntry {mappingPatternEntryKey = StringExpr "k" (Position 4 9), mappingPatternEntryValue = CapturePattern "v" (Position 4 14)}]
                          Nothing
                          (Position 4 8),
                      matchCaseGuard = Nothing,
                      matchCaseBody = [PrintStmt (IdentifierExpr "v" (Position 5 11)) (Position 5 5)],
                      matchCasePos = Position 4 3
                    },
                  MatchCase
                    { matchCasePattern = WildcardPattern (Position 6 8),
                      matchCaseGuard = Nothing,
                      matchCaseBody = [PrintStmt (IntegerExpr 0 (Position 7 11)) (Position 7 5)],
                      matchCasePos = Position 6 3
                    }
                ]
                (Position 1 1)
            ]
        )

  it "parses mapping pattern with double-star rest capture" $ do
    parseProgram
      [ Token MatchToken "match" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token CaseToken "case" (Position 2 3),
        Token LBraceToken "{" (Position 2 8),
        Token StringToken "k" (Position 2 9),
        Token ColonToken ":" (Position 2 12),
        Token IdentifierToken "v" (Position 2 14),
        Token CommaToken "," (Position 2 15),
        Token StarToken "*" (Position 2 17),
        Token StarToken "*" (Position 2 18),
        Token IdentifierToken "rest" (Position 2 19),
        Token RBraceToken "}" (Position 2 23),
        Token ColonToken ":" (Position 2 24),
        Token NewlineToken "\\n" (Position 2 25),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PrintToken "print" (Position 3 5),
        Token IdentifierToken "rest" (Position 3 11),
        Token NewlineToken "\\n" (Position 3 15),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ MatchStmt
                (IdentifierExpr "x" (Position 1 7))
                [ MatchCase
                    { matchCasePattern =
                        MappingPattern
                          [MappingPatternEntry {mappingPatternEntryKey = StringExpr "k" (Position 2 9), mappingPatternEntryValue = CapturePattern "v" (Position 2 14)}]
                          (Just "rest")
                          (Position 2 8),
                      matchCaseGuard = Nothing,
                      matchCaseBody = [PrintStmt (IdentifierExpr "rest" (Position 3 11)) (Position 3 5)],
                      matchCasePos = Position 2 3
                    }
                ]
                (Position 1 1)
            ]
        )

  it "parses match statement with as-pattern" $ do
    parseProgram
      [ Token MatchToken "match" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token CaseToken "case" (Position 2 3),
        Token LBracketToken "[" (Position 2 8),
        Token IdentifierToken "a" (Position 2 9),
        Token CommaToken "," (Position 2 10),
        Token IdentifierToken "b" (Position 2 12),
        Token RBracketToken "]" (Position 2 13),
        Token AsToken "as" (Position 2 15),
        Token IdentifierToken "pair" (Position 2 18),
        Token ColonToken ":" (Position 2 22),
        Token NewlineToken "\\n" (Position 2 23),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PrintToken "print" (Position 3 5),
        Token IdentifierToken "pair" (Position 3 11),
        Token NewlineToken "\\n" (Position 3 15),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ MatchStmt
                (IdentifierExpr "x" (Position 1 7))
                [ MatchCase
                    { matchCasePattern = AsPattern (SequencePattern [CapturePattern "a" (Position 2 9), CapturePattern "b" (Position 2 12)] Nothing (Position 2 8)) "pair" (Position 2 15),
                      matchCaseGuard = Nothing,
                      matchCaseBody = [PrintStmt (IdentifierExpr "pair" (Position 3 11)) (Position 3 5)],
                      matchCasePos = Position 2 3
                    }
                ]
                (Position 1 1)
            ]
        )
