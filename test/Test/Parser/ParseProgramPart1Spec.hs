{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart1Spec (spec) where

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
spec = describe "parseProgram - part 1" $ do
  it "parses decorated function definition with call decorator" $ do
    parseProgram
      [ Token AtToken "@" (Position 1 1),
        Token IdentifierToken "deco" (Position 1 2),
        Token LParenToken "(" (Position 1 6),
        Token IntegerToken "1" (Position 1 7),
        Token RParenToken ")" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token DefToken "def" (Position 2 1),
        Token IdentifierToken "f" (Position 2 5),
        Token LParenToken "(" (Position 2 6),
        Token IdentifierToken "x" (Position 2 7),
        Token RParenToken ")" (Position 2 8),
        Token ColonToken ":" (Position 2 9),
        Token NewlineToken "\\n" (Position 2 10),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token ReturnToken "return" (Position 3 3),
        Token IdentifierToken "x" (Position 3 10),
        Token NewlineToken "\\n" (Position 3 11),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ DecoratedStmt
                [CallExpr "deco" [IntegerExpr 1 (Position 1 7)] (Position 1 2)]
                (FunctionDefStmt "f" ["x"] [ReturnStmt (IdentifierExpr "x" (Position 3 10)) (Position 3 3)] (Position 2 1))
                (Position 1 1)
            ]
        )

  it "parses decorated class definition" $ do
    parseProgram
      [ Token AtToken "@" (Position 1 1),
        Token IdentifierToken "deco" (Position 1 2),
        Token NewlineToken "\\n" (Position 1 6),
        Token ClassToken "class" (Position 2 1),
        Token IdentifierToken "C" (Position 2 7),
        Token ColonToken ":" (Position 2 8),
        Token NewlineToken "\\n" (Position 2 9),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PassToken "pass" (Position 3 3),
        Token NewlineToken "\\n" (Position 3 7),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ DecoratedStmt
                [IdentifierExpr "deco" (Position 1 2)]
                (ClassDefStmt "C" Nothing [PassStmt (Position 3 3)] (Position 2 1))
                (Position 1 1)
            ]
        )

  it "parses class statement with single inheritance and method" $ do
    parseProgram
      [ Token ClassToken "class" (Position 1 1),
        Token IdentifierToken "B" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "A" (Position 1 9),
        Token RParenToken ")" (Position 1 10),
        Token ColonToken ":" (Position 1 11),
        Token NewlineToken "\\n" (Position 1 12),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token DefToken "def" (Position 2 3),
        Token IdentifierToken "f" (Position 2 7),
        Token LParenToken "(" (Position 2 8),
        Token IdentifierToken "self" (Position 2 9),
        Token RParenToken ")" (Position 2 13),
        Token ColonToken ":" (Position 2 14),
        Token NewlineToken "\\n" (Position 2 15),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token ReturnToken "return" (Position 3 5),
        Token IntegerToken "1" (Position 3 12),
        Token NewlineToken "\\n" (Position 3 13),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ ClassDefStmt
                "B"
                (Just "A")
                [FunctionDefStmt "f" ["self"] [ReturnStmt (IntegerExpr 1 (Position 3 12)) (Position 3 5)] (Position 2 3)]
                (Position 1 1)
            ]
        )

  it "parses decorated method inside class body" $ do
    case scanTokens "class A:\n  @staticmethod\n  def f(x): return x + 1\n" of
      Right tokens -> parseProgram tokens `shouldSatisfy` isRight
      Left _ -> expectationFailure "lexer error"

  it "parses decorated method inside class body followed by top-level statement" $ do
    case scanTokens "class A:\n  @staticmethod\n  def f(x): return x + 1\nprint A.f(2)\n" of
      Right tokens -> parseProgram tokens `shouldSatisfy` isRight
      Left _ -> expectationFailure "lexer error"

  it "parses decorated method inside class body followed by multiple top-level statements" $ do
    case scanTokens "class A:\n  @staticmethod\n  def f(x): return x + 1\nprint A.f(2)\na = A()\ng = a.f\nprint g(3)\n" of
      Right tokens -> parseProgram tokens `shouldSatisfy` isRight
      Left _ -> expectationFailure "lexer error"

  it "parses dotted assignment target as assign stmt name" $ do
    parseProgram
      [ Token IdentifierToken "self" (Position 1 1),
        Token DotToken "." (Position 1 5),
        Token IdentifierToken "x" (Position 1 6),
        Token AssignToken "=" (Position 1 8),
        Token IntegerToken "1" (Position 1 10),
        Token NewlineToken "\\n" (Position 1 11),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ AssignStmt "self.x" (IntegerExpr 1 (Position 1 10)) (Position 1 1)
            ]
        )

  it "parses import statement" $ do
    parseProgram
      [ Token ImportToken "import" (Position 1 1),
        Token IdentifierToken "math" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 12),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ ImportStmt [ImportEntry {importEntryPath = ["math"], importEntryAlias = Nothing}] (Position 1 1)
            ]
        )

  it "parses import statement with dotted module and alias" $ do
    parseProgram
      [ Token ImportToken "import" (Position 1 1),
        Token IdentifierToken "pkg" (Position 1 8),
        Token DotToken "." (Position 1 11),
        Token IdentifierToken "mod" (Position 1 12),
        Token AsToken "as" (Position 1 16),
        Token IdentifierToken "m" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ ImportStmt [ImportEntry {importEntryPath = ["pkg", "mod"], importEntryAlias = Just "m"}] (Position 1 1)
            ]
        )

  it "parses from-import statement with alias" $ do
    parseProgram
      [ Token FromToken "from" (Position 1 1),
        Token IdentifierToken "pkg" (Position 1 6),
        Token DotToken "." (Position 1 9),
        Token IdentifierToken "mod" (Position 1 10),
        Token ImportToken "import" (Position 1 14),
        Token IdentifierToken "sqrt" (Position 1 21),
        Token AsToken "as" (Position 1 26),
        Token IdentifierToken "s" (Position 1 29),
        Token NewlineToken "\\n" (Position 1 30),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FromImportStmt 0 ["pkg", "mod"] [FromImportEntry {fromImportEntryName = "sqrt", fromImportEntryAlias = Just "s"}] (Position 1 1)
            ]
        )
