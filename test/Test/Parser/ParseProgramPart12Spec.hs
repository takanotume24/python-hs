{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart12Spec (spec) where

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
spec = describe "parseProgram - part 12" $ do
  it "parses an indented for suite with multiple statements" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "item" (Position 1 5),
        Token InToken "in" (Position 1 10),
        Token IdentifierToken "items" (Position 1 13),
        Token ColonToken ":" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token PrintToken "print" (Position 3 3),
        Token IdentifierToken "item" (Position 3 9),
        Token NewlineToken "\\n" (Position 3 13),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ ForStmt
                "item"
                (IdentifierExpr "items" (Position 1 13))
                [ PassStmt (Position 2 3),
                  PrintStmt (IdentifierExpr "item" (Position 3 9)) (Position 3 3)
                ]
                Nothing
                (Position 1 1)
            ]
        )

  it "parses nested conditional inside for suite and continues" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "item" (Position 1 5),
        Token InToken "in" (Position 1 10),
        Token IdentifierToken "items" (Position 1 13),
        Token ColonToken ":" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
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
            [ ForStmt
                "item"
                (IdentifierExpr "items" (Position 1 13))
                [ IfStmt
                    (IdentifierExpr "inner" (Position 2 6))
                    [PrintStmt (IntegerExpr 1 (Position 3 11)) (Position 3 5)]
                    (Just [PrintStmt (IntegerExpr 2 (Position 5 11)) (Position 5 5)])
                    (Position 2 3),
                  PassStmt (Position 6 3)
                ]
                Nothing
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 7 7)) (Position 7 1)
            ]
        )

  it "parses break and continue statements" $ do
    parseProgram
      [ Token BreakToken "break" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 6),
        Token ContinueToken "continue" (Position 2 1),
        Token NewlineToken "\\n" (Position 2 9),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldBe` Right (Program [BreakStmt (Position 1 1), ContinueStmt (Position 2 1)])

  it "parses list literal expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token CommaToken "," (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token RBracketToken "]" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (ListExpr [IntegerExpr 1 (Position 1 8), IntegerExpr 2 (Position 1 11)] (Position 1 7)) (Position 1 1)])

  it "parses lambda expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LambdaToken "lambda" (Position 1 7),
        Token IdentifierToken "x" (Position 1 14),
        Token ColonToken ":" (Position 1 15),
        Token IdentifierToken "x" (Position 1 17),
        Token PlusToken "+" (Position 1 19),
        Token IntegerToken "1" (Position 1 21),
        Token NewlineToken "\\n" (Position 1 22),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (LambdaExpr ["x"] (BinaryExpr AddOperator (IdentifierExpr "x" (Position 1 17)) (IntegerExpr 1 (Position 1 21)) (Position 1 19)) (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses list comprehension expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IdentifierToken "x" (Position 1 8),
        Token ForToken "for" (Position 1 10),
        Token IdentifierToken "x" (Position 1 14),
        Token InToken "in" (Position 1 16),
        Token IdentifierToken "xs" (Position 1 19),
        Token RBracketToken "]" (Position 1 21),
        Token NewlineToken "\\n" (Position 1 22),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (ListComprehensionExpr (IdentifierExpr "x" (Position 1 8)) "x" (IdentifierExpr "xs" (Position 1 19)) (Position 1 7))
                (Position 1 1)
            ]
        )
