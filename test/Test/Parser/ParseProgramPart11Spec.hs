{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart11Spec (spec) where

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
spec = describe "parseProgram - part 11" $ do
  it "parses return inside function body" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "id" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token ReturnToken "return" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [FunctionDefStmt "id" ["x"] [ReturnStmt (IdentifierExpr "x" (Position 1 1)) (Position 1 1)] (Position 1 1)])

  it "parses bare return inside function body as None" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "id" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token ReturnToken "return" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [FunctionDefStmt "id" ["x"] [ReturnStmt (NoneExpr (Position 1 1)) (Position 1 1)] (Position 1 1)])

  it "parses yield inside function body" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "gen" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token RParenToken ")" (Position 1 9),
        Token ColonToken ":" (Position 1 10),
        Token YieldToken "yield" (Position 1 12),
        Token IntegerToken "1" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [FunctionDefStmt "gen" [] [YieldStmt (IntegerExpr 1 (Position 1 18)) (Position 1 12)] (Position 1 1)])

  it "parses yield from inside function body" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "gen" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token RParenToken ")" (Position 1 9),
        Token ColonToken ":" (Position 1 10),
        Token YieldToken "yield" (Position 1 12),
        Token FromToken "from" (Position 1 18),
        Token IdentifierToken "xs" (Position 1 23),
        Token NewlineToken "\\n" (Position 1 25),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [FunctionDefStmt "gen" [] [YieldFromStmt (IdentifierExpr "xs" (Position 1 23)) (Position 1 12)] (Position 1 1)])

  it "parses an indented function suite with multiple statements" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token RParenToken ")" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token ReturnToken "return" (Position 3 3),
        Token IntegerToken "1" (Position 3 10),
        Token NewlineToken "\\n" (Position 3 11),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefStmt
                "f"
                []
                [ PassStmt (Position 2 3),
                  ReturnStmt (IntegerExpr 1 (Position 3 10)) (Position 3 3)
                ]
                (Position 1 1)
            ]
        )

  it "parses comparison with precedence (1 + 2 == 3)" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token PlusToken "+" (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token EqToken "==" (Position 1 1),
        Token IntegerToken "3" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [PrintStmt (BinaryExpr EqOperator (BinaryExpr AddOperator (IntegerExpr 1 (Position 1 1)) (IntegerExpr 2 (Position 1 1)) (Position 1 1)) (IntegerExpr 3 (Position 1 1)) (Position 1 1)) (Position 1 1)])

  it "parses not / and / or precedence" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token NotToken "not" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [PrintStmt (NotExpr (IdentifierExpr "x" (Position 1 1)) (Position 1 1)) (Position 1 1)])

  it "parses and/or as lower-precedence than comparison" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token AndToken "and" (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token OrToken "or" (Position 1 1),
        Token IdentifierToken "c" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [PrintStmt (BinaryExpr OrOperator (BinaryExpr AndOperator (IdentifierExpr "a" (Position 1 1)) (IdentifierExpr "b" (Position 1 1)) (Position 1 1)) (IdentifierExpr "c" (Position 1 1)) (Position 1 1)) (Position 1 1)])

  it "parses string literal expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token StringToken "hello" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (StringExpr "hello" (Position 1 7)) (Position 1 1)])

  it "parses True/False/None literals" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token TrueToken "True" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 11),
        Token PrintToken "print" (Position 2 1),
        Token FalseToken "False" (Position 2 7),
        Token NewlineToken "\\n" (Position 2 12),
        Token PrintToken "print" (Position 3 1),
        Token NoneToken "None" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 11),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt (IntegerExpr 1 (Position 1 7)) (Position 1 1),
              PrintStmt (IntegerExpr 0 (Position 2 7)) (Position 2 1),
              PrintStmt (NoneExpr (Position 3 7)) (Position 3 1)
            ]
        )

  it "parses for statement" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "i" (Position 1 5),
        Token InToken "in" (Position 1 7),
        Token IntegerToken "3" (Position 1 10),
        Token ColonToken ":" (Position 1 11),
        Token PrintToken "print" (Position 1 13),
        Token IdentifierToken "i" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [ForStmt "i" (IntegerExpr 3 (Position 1 10)) [PrintStmt (IdentifierExpr "i" (Position 1 19)) (Position 1 13)] Nothing (Position 1 1)])
