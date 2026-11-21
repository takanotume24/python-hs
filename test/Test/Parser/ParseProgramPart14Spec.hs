{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart14Spec (spec) where

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
spec = describe "parseProgram - part 14" $ do
  it "parses multiplicative operators with precedence" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token PlusToken "+" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token StarToken "*" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr AddOperator (IntegerExpr 1 (Position 1 7)) (BinaryExpr MultiplyOperator (IntegerExpr 2 (Position 1 11)) (IntegerExpr 3 (Position 1 15)) (Position 1 13)) (Position 1 9))
                (Position 1 1)
            ]
        )

    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "8" (Position 1 7),
        Token SlashToken "/" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token PercentToken "%" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr ModuloOperator (BinaryExpr DivideOperator (IntegerExpr 8 (Position 1 7)) (IntegerExpr 2 (Position 1 11)) (Position 1 9)) (IntegerExpr 3 (Position 1 15)) (Position 1 13))
                (Position 1 1)
            ]
        )

  describe "with statement" $ do
    it "parses simple with statement" $ do
      parseProgram
        [ Token WithToken "with" (Position 1 1),
          Token IdentifierToken "open" (Position 1 6),
          Token LParenToken "(" (Position 1 10),
          Token StringToken "'file.txt'" (Position 1 11),
          Token RParenToken ")" (Position 1 21),
          Token ColonToken ":" (Position 1 22),
          Token NewlineToken "\n" (Position 1 23),
          Token IndentToken "<INDENT>" (Position 2 1),
          Token PrintToken "print" (Position 2 3),
          Token IntegerToken "1" (Position 2 9),
          Token NewlineToken "\n" (Position 2 10),
          Token DedentToken "<DEDENT>" (Position 3 1),
          Token EOFToken "" (Position 3 1)
        ]
        `shouldBe` Right (Program [WithStmt (CallExpr "open" [StringExpr "'file.txt'" (Position 1 11)] (Position 1 6)) Nothing [PrintStmt (IntegerExpr 1 (Position 2 9)) (Position 2 3)] (Position 1 1)])

    it "parses with statement with complex expression" $ do
      parseProgram
        [ Token WithToken "with" (Position 1 1),
          Token IdentifierToken "open" (Position 1 6),
          Token LParenToken "(" (Position 1 10),
          Token StringToken "'file.txt'" (Position 1 11),
          Token RParenToken ")" (Position 1 21),
          Token AsToken "as" (Position 1 23),
          Token IdentifierToken "f" (Position 1 26),
          Token ColonToken ":" (Position 1 27),
          Token NewlineToken "\n" (Position 1 28),
          Token IndentToken "<INDENT>" (Position 2 1),
          Token PrintToken "print" (Position 2 3),
          Token IdentifierToken "f" (Position 2 9),
          Token NewlineToken "\n" (Position 2 10),
          Token DedentToken "<DEDENT>" (Position 3 1),
          Token EOFToken "" (Position 3 1)
        ]
        `shouldBe` Right (Program [WithStmt (CallExpr "open" [StringExpr "'file.txt'" (Position 1 11)] (Position 1 6)) (Just "f") [PrintStmt (IdentifierExpr "f" (Position 2 9)) (Position 2 3)] (Position 1 1)])

    it "parses with statement with multiple statements in body" $ do
      parseProgram
        [ Token WithToken "with" (Position 1 1),
          Token IdentifierToken "open" (Position 1 6),
          Token LParenToken "(" (Position 1 10),
          Token StringToken "'file.txt'" (Position 1 11),
          Token RParenToken ")" (Position 1 21),
          Token ColonToken ":" (Position 1 22),
          Token NewlineToken "\n" (Position 1 23),
          Token IndentToken "<INDENT>" (Position 2 1),
          Token PrintToken "print" (Position 2 3),
          Token IntegerToken "1" (Position 2 9),
          Token NewlineToken "\n" (Position 2 10),
          Token PrintToken "print" (Position 3 3),
          Token IntegerToken "2" (Position 3 9),
          Token NewlineToken "\n" (Position 3 10),
          Token DedentToken "<DEDENT>" (Position 4 1),
          Token EOFToken "" (Position 4 1)
        ]
        `shouldBe` Right (Program [WithStmt (CallExpr "open" [StringExpr "'file.txt'" (Position 1 11)] (Position 1 6)) Nothing [PrintStmt (IntegerExpr 1 (Position 2 9)) (Position 2 3), PrintStmt (IntegerExpr 2 (Position 3 9)) (Position 3 3)] (Position 1 1)])

    it "parses in operator expression" $
      parseProgram
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "1" (Position 1 7),
          Token InToken "in" (Position 1 9),
          Token LBracketToken "[" (Position 1 12),
          Token IntegerToken "1" (Position 1 13),
          Token CommaToken "," (Position 1 14),
          Token IntegerToken "2" (Position 1 16),
          Token RBracketToken "]" (Position 1 17),
          Token NewlineToken "\n" (Position 1 18),
          Token EOFToken "" (Position 2 1)
        ]
        `shouldBe` Right (Program [PrintStmt (BinaryExpr {binaryExprOp = InOperator, binaryExprLeft = IntegerExpr 1 (Position 1 7), binaryExprRight = ListExpr [IntegerExpr 1 (Position 1 13), IntegerExpr 2 (Position 1 16)] (Position 1 12), binaryExprPos = Position 1 9}) (Position 1 1)])

    it "parses not in operator expression" $
      parseProgram
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "1" (Position 1 7),
          Token NotToken "not" (Position 1 9),
          Token InToken "in" (Position 1 13),
          Token LBracketToken "[" (Position 1 16),
          Token IntegerToken "1" (Position 1 17),
          Token CommaToken "," (Position 1 18),
          Token IntegerToken "2" (Position 1 20),
          Token RBracketToken "]" (Position 1 21),
          Token NewlineToken "\n" (Position 1 22),
          Token EOFToken "" (Position 2 1)
        ]
        `shouldBe` Right (Program [PrintStmt (BinaryExpr {binaryExprOp = NotInOperator, binaryExprLeft = IntegerExpr 1 (Position 1 7), binaryExprRight = ListExpr [IntegerExpr 1 (Position 1 17), IntegerExpr 2 (Position 1 20)] (Position 1 16), binaryExprPos = Position 1 9}) (Position 1 1)])

    it "parses is operator expression" $
      parseProgram
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "1" (Position 1 7),
          Token IsToken "is" (Position 1 9),
          Token IntegerToken "1" (Position 1 12),
          Token NewlineToken "\n" (Position 1 13),
          Token EOFToken "" (Position 2 1)
        ]
        `shouldBe` Right (Program [PrintStmt (BinaryExpr {binaryExprOp = IsOperator, binaryExprLeft = IntegerExpr 1 (Position 1 7), binaryExprRight = IntegerExpr 1 (Position 1 12), binaryExprPos = Position 1 9}) (Position 1 1)])

    it "parses is not operator expression" $
      parseProgram
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "1" (Position 1 7),
          Token IsToken "is" (Position 1 9),
          Token NotToken "not" (Position 1 12),
          Token IntegerToken "2" (Position 1 16),
          Token NewlineToken "\n" (Position 1 17),
          Token EOFToken "" (Position 2 1)
        ]
        `shouldBe` Right (Program [PrintStmt (BinaryExpr {binaryExprOp = IsNotOperator, binaryExprLeft = IntegerExpr 1 (Position 1 7), binaryExprRight = IntegerExpr 2 (Position 1 16), binaryExprPos = Position 1 9}) (Position 1 1)])

    it "parses bitwise and expression" $
      parseProgram
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "5" (Position 1 7),
          Token AmpersandToken "&" (Position 1 9),
          Token IntegerToken "3" (Position 1 11),
          Token NewlineToken "\n" (Position 1 12),
          Token EOFToken "" (Position 2 1)
        ]
        `shouldBe` Right (Program [PrintStmt (BinaryExpr {binaryExprOp = BitwiseAndOperator, binaryExprLeft = IntegerExpr 5 (Position 1 7), binaryExprRight = IntegerExpr 3 (Position 1 11), binaryExprPos = Position 1 9}) (Position 1 1)])

    it "parses bitwise not expression" $
      parseProgram
        [ Token PrintToken "print" (Position 1 1),
          Token TildeToken "~" (Position 1 7),
          Token IntegerToken "5" (Position 1 8),
          Token NewlineToken "\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]
        `shouldBe` Right (Program [PrintStmt (BitwiseNotExpr {bitwiseNotExprValue = IntegerExpr 5 (Position 1 8), bitwiseNotExprPos = Position 1 7}) (Position 1 1)])
