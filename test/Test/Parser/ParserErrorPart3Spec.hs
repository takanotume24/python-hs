{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParserErrorPart3Spec (spec) where

import Data.Either (isRight)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
import PythonHS.Parser.ParseError (ParseError (ExpectedAssignAfterIdentifier, ExpectedExpression, ExpectedNewlineAfterStatement))
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "parse error reporting - part 3" $ do
  it "accepts type annotation in function parameter" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token IdentifierToken "a" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token IdentifierToken "int" (Position 1 10),
        Token RParenToken ")" (Position 1 13),
        Token ColonToken ":" (Position 1 14),
        Token NewlineToken "\\n" (Position 1 15),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldSatisfy` isRight

  it "accepts return type annotation in function definition" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token RParenToken ")" (Position 1 7),
        Token MinusToken "-" (Position 1 9),
        Token GtToken ">" (Position 1 10),
        Token IdentifierToken "int" (Position 1 12),
        Token ColonToken ":" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldSatisfy` isRight

  it "reports ExpectedExpression when duplicate parameter names include defaults" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "dup" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IdentifierToken "a" (Position 1 14),
        Token AssignToken "=" (Position 1 15),
        Token IntegerToken "2" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token ColonToken ":" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 14))
