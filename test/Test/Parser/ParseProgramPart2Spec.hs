{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParseProgramPart2Spec (spec) where

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
spec = describe "parseProgram - part 2" $ do
  it "parses relative from-import statement" $ do
    parseProgram
      [ Token FromToken "from" (Position 1 1),
        Token DotToken "." (Position 1 6),
        Token DotToken "." (Position 1 7),
        Token IdentifierToken "pkg" (Position 1 8),
        Token ImportToken "import" (Position 1 12),
        Token IdentifierToken "x" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FromImportStmt 2 ["pkg"] [FromImportEntry {fromImportEntryName = "x", fromImportEntryAlias = Nothing}] (Position 1 1)
            ]
        )

  it "parses relative from-import statement without module path" $ do
    parseProgram
      [ Token FromToken "from" (Position 1 1),
        Token DotToken "." (Position 1 6),
        Token ImportToken "import" (Position 1 8),
        Token IdentifierToken "x" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FromImportStmt 1 [] [FromImportEntry {fromImportEntryName = "x", fromImportEntryAlias = Nothing}] (Position 1 1)
            ]
        )

  it "parses from-import star" $ do
    parseProgram
      [ Token FromToken "from" (Position 1 1),
        Token IdentifierToken "pkg" (Position 1 6),
        Token ImportToken "import" (Position 1 10),
        Token StarToken "*" (Position 1 17),
        Token NewlineToken "\\n" (Position 1 18),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FromImportStmt 0 ["pkg"] [FromImportEntry {fromImportEntryName = "*", fromImportEntryAlias = Nothing}] (Position 1 1)
            ]
        )

  it "parses parenthesized from-import with trailing comma" $ do
    parseProgram
      [ Token FromToken "from" (Position 1 1),
        Token IdentifierToken "pkg" (Position 1 6),
        Token ImportToken "import" (Position 1 10),
        Token LParenToken "(" (Position 1 17),
        Token IdentifierToken "a" (Position 1 18),
        Token CommaToken "," (Position 1 19),
        Token IdentifierToken "b" (Position 1 21),
        Token AsToken "as" (Position 1 23),
        Token IdentifierToken "c" (Position 1 26),
        Token CommaToken "," (Position 1 27),
        Token RParenToken ")" (Position 1 28),
        Token NewlineToken "\\n" (Position 1 29),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FromImportStmt 0 ["pkg"] [FromImportEntry {fromImportEntryName = "a", fromImportEntryAlias = Nothing}, FromImportEntry {fromImportEntryName = "b", fromImportEntryAlias = Just "c"}] (Position 1 1)
            ]
        )

  it "parses raise statement" $ do
    parseProgram
      [ Token RaiseToken "raise" (Position 1 1),
        Token StringToken "oops" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ RaiseStmt (Just (StringExpr "oops" (Position 1 7))) (Position 1 1)
            ]
        )

  it "parses bare raise statement" $ do
    parseProgram
      [ Token RaiseToken "raise" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 6),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ RaiseStmt Nothing (Position 1 1)
            ]
        )

  it "parses try/except statement with indented suites" $ do
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
        Token EOFToken "" (Position 5 1)
      ]
      `shouldBe` Right
        ( Program
            [ TryExceptStmt
                [RaiseStmt (Just (StringExpr "x" (Position 2 9))) (Position 2 3)]
                [ExceptSuite {exceptSuiteType = Nothing, exceptSuiteAlias = Nothing, exceptSuiteBody = [PrintStmt (IntegerExpr 1 (Position 4 9)) (Position 4 3)], exceptSuitePos = Position 3 1}]
                Nothing
                Nothing
                (Position 1 1)
            ]
        )

  it "parses try/except/finally statement with indented suites" $ do
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
        Token ColonToken ":" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PrintToken "print" (Position 4 3),
        Token IntegerToken "2" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token FinallyToken "finally" (Position 5 1),
        Token ColonToken ":" (Position 5 8),
        Token NewlineToken "\\n" (Position 5 9),
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
                [ExceptSuite {exceptSuiteType = Nothing, exceptSuiteAlias = Nothing, exceptSuiteBody = [PrintStmt (IntegerExpr 2 (Position 4 9)) (Position 4 3)], exceptSuitePos = Position 3 1}]
                Nothing
                (Just [PrintStmt (IntegerExpr 3 (Position 6 9)) (Position 6 3)])
                (Position 1 1)
            ]
        )
