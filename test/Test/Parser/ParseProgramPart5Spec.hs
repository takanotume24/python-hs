{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart5Spec (spec) where

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
spec = describe "parseProgram - part 5" $ do
  it "parses float literal expressions" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token FloatToken "1.5" (Position 1 7),
        Token PlusToken "+" (Position 1 11),
        Token IntegerToken "2" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr AddOperator (FloatExpr 1.5 (Position 1 7)) (IntegerExpr 2 (Position 1 13)) (Position 1 11))
                (Position 1 1)
            ]
        )

  it "parses assignment and print statements" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token AssignToken "=" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token PlusToken "+" (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right
        ( Program
            [ AssignStmt "x" (IntegerExpr 1 (Position 1 1)) (Position 1 1),
              PrintStmt (BinaryExpr AddOperator (IdentifierExpr "x" (Position 1 1)) (IntegerExpr 2 (Position 1 1)) (Position 1 1)) (Position 1 1)
            ]
        )

  it "parses plus-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token PlusAssignToken "+=" (Position 1 3),
        Token IntegerToken "2" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ AddAssignStmt "x" (IntegerExpr 2 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses minus-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token MinusAssignToken "-=" (Position 1 3),
        Token IntegerToken "2" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ SubAssignStmt "x" (IntegerExpr 2 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses star-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token StarAssignToken "*=" (Position 1 3),
        Token IntegerToken "3" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ MulAssignStmt "x" (IntegerExpr 3 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses slash-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token SlashAssignToken "/=" (Position 1 3),
        Token IntegerToken "3" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ DivAssignStmt "x" (IntegerExpr 3 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses percent-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token PercentAssignToken "%=" (Position 1 3),
        Token IntegerToken "3" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ ModAssignStmt "x" (IntegerExpr 3 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses double-slash-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token DoubleSlashAssignToken "//=" (Position 1 3),
        Token IntegerToken "3" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 8),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FloorDivAssignStmt "x" (IntegerExpr 3 (Position 1 7)) (Position 1 1)
            ]
        )

  it "returns parse error for invalid expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 6),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 6))

  it "parses an if statement without else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [IfStmt (IdentifierExpr "x" (Position 1 1)) [PrintStmt (IdentifierExpr "x" (Position 1 1)) (Position 1 1)] Nothing (Position 1 1)])

  it "parses an if statement with else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token ElseToken "else" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [IfStmt (IdentifierExpr "x" (Position 1 1)) [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Just [PrintStmt (IdentifierExpr "b" (Position 1 1)) (Position 1 1)]) (Position 1 1)])
