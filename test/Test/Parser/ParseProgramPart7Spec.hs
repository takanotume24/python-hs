{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart7Spec (spec) where

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
spec = describe "parseProgram - part 7" $ do
  it "parses nested conditional suites inside elif branch and continues" $ do
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
        Token IfToken "if" (Position 4 3),
        Token IdentifierToken "inner" (Position 4 6),
        Token ColonToken ":" (Position 4 11),
        Token NewlineToken "\\n" (Position 4 12),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PrintToken "print" (Position 5 5),
        Token IntegerToken "1" (Position 5 11),
        Token NewlineToken "\\n" (Position 5 12),
        Token DedentToken "<DEDENT>" (Position 6 1),
        Token ElseToken "else" (Position 6 3),
        Token ColonToken ":" (Position 6 7),
        Token NewlineToken "\\n" (Position 6 8),
        Token IndentToken "<INDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 5),
        Token IntegerToken "2" (Position 7 11),
        Token NewlineToken "\\n" (Position 7 12),
        Token DedentToken "<DEDENT>" (Position 8 1),
        Token DedentToken "<DEDENT>" (Position 8 1),
        Token ElseToken "else" (Position 8 1),
        Token ColonToken ":" (Position 8 5),
        Token NewlineToken "\\n" (Position 8 6),
        Token IndentToken "<INDENT>" (Position 9 1),
        Token PrintToken "print" (Position 9 3),
        Token IntegerToken "3" (Position 9 9),
        Token NewlineToken "\\n" (Position 9 10),
        Token DedentToken "<DEDENT>" (Position 10 1),
        Token PrintToken "print" (Position 10 1),
        Token IntegerToken "9" (Position 10 7),
        Token NewlineToken "\\n" (Position 10 8),
        Token EOFToken "" (Position 11 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [PassStmt (Position 2 3)]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "alt" (Position 3 6))
                        [ IfStmt
                            (IdentifierExpr "inner" (Position 4 6))
                            [PrintStmt (IntegerExpr 1 (Position 5 11)) (Position 5 5)]
                            (Just [PrintStmt (IntegerExpr 2 (Position 7 11)) (Position 7 5)])
                            (Position 4 3)
                        ]
                        (Just [PrintStmt (IntegerExpr 3 (Position 9 9)) (Position 9 3)])
                        (Position 3 1)
                    ]
                )
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 10 7)) (Position 10 1)
            ]
        )

  it "parses an indented if suite with multiple statements" $ do
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
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [ PassStmt (Position 2 3),
                  AssignStmt "x" (IntegerExpr 1 (Position 3 7)) (Position 3 3)
                ]
                Nothing
                (Position 1 1)
            ]
        )

  it "parses a while statement" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [WhileStmt (IdentifierExpr "cond" (Position 1 1)) [PrintStmt (IdentifierExpr "cond" (Position 1 1)) (Position 1 1)] Nothing (Position 1 1)])

  it "parses an indented while suite with multiple statements" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 7),
        Token ColonToken ":" (Position 1 11),
        Token NewlineToken "\\n" (Position 1 12),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token IdentifierToken "x" (Position 3 3),
        Token AssignToken "=" (Position 3 5),
        Token IntegerToken "1" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ WhileStmt
                (IdentifierExpr "cond" (Position 1 7))
                [ PassStmt (Position 2 3),
                  AssignStmt "x" (IntegerExpr 1 (Position 3 7)) (Position 3 3)
                ]
                Nothing
                (Position 1 1)
            ]
        )
