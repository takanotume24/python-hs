{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart13Spec (spec) where

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
spec = describe "parseProgram - part 13" $ do
  it "parses list comprehension with unpack target and if clause" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IdentifierToken "a" (Position 1 8),
        Token PlusToken "+" (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token ForToken "for" (Position 1 14),
        Token IdentifierToken "a" (Position 1 18),
        Token CommaToken "," (Position 1 19),
        Token IdentifierToken "b" (Position 1 21),
        Token InToken "in" (Position 1 23),
        Token IdentifierToken "pairs" (Position 1 26),
        Token IfToken "if" (Position 1 32),
        Token IdentifierToken "a" (Position 1 35),
        Token GtToken ">" (Position 1 37),
        Token IntegerToken "0" (Position 1 39),
        Token RBracketToken "]" (Position 1 40),
        Token NewlineToken "\\n" (Position 1 41),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( ListComprehensionClausesExpr
                    (BinaryExpr AddOperator (IdentifierExpr "a" (Position 1 8)) (IdentifierExpr "b" (Position 1 12)) (Position 1 10))
                    [ ListComprehensionClause {listComprehensionClauseTargets = ["a", "b"], listComprehensionClauseIter = IdentifierExpr "pairs" (Position 1 26), listComprehensionClauseConds = [BinaryExpr GtOperator (IdentifierExpr "a" (Position 1 35)) (IntegerExpr 0 (Position 1 39)) (Position 1 37)]}
                    ]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses walrus expression in comprehension condition" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IdentifierToken "x" (Position 1 8),
        Token ForToken "for" (Position 1 10),
        Token IdentifierToken "x" (Position 1 14),
        Token InToken "in" (Position 1 16),
        Token IdentifierToken "xs" (Position 1 19),
        Token IfToken "if" (Position 1 22),
        Token LParenToken "(" (Position 1 25),
        Token IdentifierToken "y" (Position 1 26),
        Token ColonAssignToken ":=" (Position 1 28),
        Token IdentifierToken "x" (Position 1 31),
        Token RParenToken ")" (Position 1 32),
        Token RBracketToken "]" (Position 1 33),
        Token NewlineToken "\\n" (Position 1 34),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( ListComprehensionClausesExpr
                    (IdentifierExpr "x" (Position 1 8))
                    [ListComprehensionClause {listComprehensionClauseTargets = ["x"], listComprehensionClauseIter = IdentifierExpr "xs" (Position 1 19), listComprehensionClauseConds = [WalrusExpr "y" (IdentifierExpr "x" (Position 1 31)) (Position 1 26)]}]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses list literal expression with trailing comma" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token CommaToken "," (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token RBracketToken "]" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (ListExpr [IntegerExpr 1 (Position 1 8), IntegerExpr 2 (Position 1 11)] (Position 1 7)) (Position 1 1)])

  it "parses dictionary literal expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBraceToken "{" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token ColonToken ":" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IntegerToken "3" (Position 1 14),
        Token ColonToken ":" (Position 1 15),
        Token IntegerToken "4" (Position 1 17),
        Token RBraceToken "}" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 1 8), dictEntryValue = IntegerExpr 2 (Position 1 11)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 1 14), dictEntryValue = IntegerExpr 4 (Position 1 17)}] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses dictionary literal expression with trailing comma" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBraceToken "{" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token ColonToken ":" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token RBraceToken "}" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 1 8), dictEntryValue = IntegerExpr 2 (Position 1 11)}] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses unary minus integer literal" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token MinusToken "-" (Position 1 7),
        Token IntegerToken "2" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (IntegerExpr (-2) (Position 1 7)) (Position 1 1)])

  it "parses unary minus identifier and parenthesized expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token MinusToken "-" (Position 1 7),
        Token IdentifierToken "x" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token PrintToken "print" (Position 2 1),
        Token MinusToken "-" (Position 2 7),
        Token LParenToken "(" (Position 2 8),
        Token IntegerToken "1" (Position 2 9),
        Token PlusToken "+" (Position 2 10),
        Token IntegerToken "2" (Position 2 11),
        Token RParenToken ")" (Position 2 12),
        Token NewlineToken "\\n" (Position 2 13),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt (UnaryMinusExpr (IdentifierExpr "x" (Position 1 8)) (Position 1 7)) (Position 1 1),
              PrintStmt (UnaryMinusExpr (BinaryExpr AddOperator (IntegerExpr 1 (Position 2 9)) (IntegerExpr 2 (Position 2 11)) (Position 2 10)) (Position 2 7)) (Position 2 1)
            ]
        )
