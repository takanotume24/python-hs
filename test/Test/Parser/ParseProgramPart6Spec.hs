{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart6Spec (spec) where

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
spec = describe "parseProgram - part 6" $ do
  it "parses an if statement with newline before else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token ElseToken "else" (Position 2 1),
        Token ColonToken ":" (Position 2 1),
        Token PrintToken "print" (Position 2 1),
        Token IdentifierToken "b" (Position 2 1),
        Token NewlineToken "\\n" (Position 2 1),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [IfStmt (IdentifierExpr "x" (Position 1 1)) [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Just [PrintStmt (IdentifierExpr "b" (Position 2 1)) (Position 2 1)]) (Position 1 1)])

  it "parses an if statement with elif and else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 4),
        Token ColonToken ":" (Position 1 5),
        Token PrintToken "print" (Position 2 1),
        Token IntegerToken "1" (Position 2 7),
        Token NewlineToken "\\n" (Position 2 8),
        Token ElifToken "elif" (Position 3 1),
        Token IdentifierToken "y" (Position 3 6),
        Token ColonToken ":" (Position 3 7),
        Token PrintToken "print" (Position 4 1),
        Token IntegerToken "2" (Position 4 7),
        Token NewlineToken "\\n" (Position 4 8),
        Token ElseToken "else" (Position 5 1),
        Token ColonToken ":" (Position 5 5),
        Token PrintToken "print" (Position 6 1),
        Token IntegerToken "3" (Position 6 7),
        Token NewlineToken "\\n" (Position 6 8),
        Token EOFToken "" (Position 7 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "x" (Position 1 4))
                [PrintStmt (IntegerExpr 1 (Position 2 7)) (Position 2 1)]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "y" (Position 3 6))
                        [PrintStmt (IntegerExpr 2 (Position 4 7)) (Position 4 1)]
                        (Just [PrintStmt (IntegerExpr 3 (Position 6 7)) (Position 6 1)])
                        (Position 3 1)
                    ]
                )
                (Position 1 1)
            ]
        )

  it "parses indented if/elif/else suites with multiple statements" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 4),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token IdentifierToken "x" (Position 3 3),
        Token AssignToken "=" (Position 3 5),
        Token IntegerToken "1" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token ElifToken "elif" (Position 4 1),
        Token IdentifierToken "alt" (Position 4 6),
        Token ColonToken ":" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PassToken "pass" (Position 5 3),
        Token NewlineToken "\\n" (Position 5 7),
        Token IdentifierToken "x" (Position 6 3),
        Token AssignToken "=" (Position 6 5),
        Token IntegerToken "2" (Position 6 7),
        Token NewlineToken "\\n" (Position 6 8),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token ElseToken "else" (Position 7 1),
        Token ColonToken ":" (Position 7 5),
        Token NewlineToken "\\n" (Position 7 6),
        Token IndentToken "<INDENT>" (Position 8 1),
        Token PassToken "pass" (Position 8 3),
        Token NewlineToken "\\n" (Position 8 7),
        Token IdentifierToken "x" (Position 9 3),
        Token AssignToken "=" (Position 9 5),
        Token IntegerToken "3" (Position 9 7),
        Token NewlineToken "\\n" (Position 9 8),
        Token DedentToken "<DEDENT>" (Position 10 1),
        Token EOFToken "" (Position 10 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [ PassStmt (Position 2 3),
                  AssignStmt "x" (IntegerExpr 1 (Position 3 7)) (Position 3 3)
                ]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "alt" (Position 4 6))
                        [ PassStmt (Position 5 3),
                          AssignStmt "x" (IntegerExpr 2 (Position 6 7)) (Position 6 3)
                        ]
                        ( Just
                            [ PassStmt (Position 8 3),
                              AssignStmt "x" (IntegerExpr 3 (Position 9 7)) (Position 9 3)
                            ]
                        )
                        (Position 4 1)
                    ]
                )
                (Position 1 1)
            ]
        )

  it "parses a statement after indented if/elif/else suites" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 4),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ElifToken "elif" (Position 3 1),
        Token IdentifierToken "alt" (Position 3 6),
        Token ColonToken ":" (Position 3 9),
        Token NewlineToken "\\n" (Position 3 10),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PassToken "pass" (Position 4 3),
        Token NewlineToken "\\n" (Position 4 7),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token ElseToken "else" (Position 5 1),
        Token ColonToken ":" (Position 5 5),
        Token NewlineToken "\\n" (Position 5 6),
        Token IndentToken "<INDENT>" (Position 6 1),
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
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [PassStmt (Position 2 3)]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "alt" (Position 3 6))
                        [PassStmt (Position 4 3)]
                        (Just [PassStmt (Position 6 3)])
                        (Position 3 1)
                    ]
                )
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 7 7)) (Position 7 1)
            ]
        )
