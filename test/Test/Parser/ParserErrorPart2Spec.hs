{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Parser.ParserErrorPart2Spec (spec) where

import Data.Either (isRight)
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
import PythonHS.Parser.ParseError (ParseError (ExpectedAssignAfterIdentifier, ExpectedExpression, ExpectedNewlineAfterStatement))
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "parse error reporting - part 2" $ do
  it "reports ExpectedExpression when required parameter appears after default parameter" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "bad" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IdentifierToken "b" (Position 1 14),
        Token RParenToken ")" (Position 1 15),
        Token ColonToken ":" (Position 1 16),
        Token NewlineToken "\\n" (Position 1 17),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 14))

  it "reports ExpectedExpression when function parameters contain duplicate names" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "dup" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "a" (Position 1 12),
        Token RParenToken ")" (Position 1 13),
        Token ColonToken ":" (Position 1 14),
        Token NewlineToken "\\n" (Position 1 15),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 12))

  it "reports ExpectedExpression for mixed keyword then positional call arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IntegerToken "2" (Position 1 14),
        Token RParenToken ")" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 14))

  it "reports ExpectedExpression when positional argument appears after multiple keyword arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IdentifierToken "b" (Position 1 14),
        Token AssignToken "=" (Position 1 15),
        Token IntegerToken "2" (Position 1 16),
        Token CommaToken "," (Position 1 17),
        Token IntegerToken "3" (Position 1 19),
        Token RParenToken ")" (Position 1 20),
        Token NewlineToken "\\n" (Position 1 21),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 19))

  it "accepts star expansion in call arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token StarToken "*" (Position 1 9),
        Token LBracketToken "[" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IntegerToken "2" (Position 1 13),
        Token RBracketToken "]" (Position 1 14),
        Token RParenToken ")" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldSatisfy` isRight

  it "accepts double-star expansion in call arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token StarToken "*" (Position 1 9),
        Token StarToken "*" (Position 1 10),
        Token LBraceToken "{" (Position 1 11),
        Token RBraceToken "}" (Position 1 12),
        Token RParenToken ")" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldSatisfy` isRight

  it "accepts *args in function definition" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token StarToken "*" (Position 1 7),
        Token IdentifierToken "args" (Position 1 8),
        Token RParenToken ")" (Position 1 12),
        Token ColonToken ":" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldSatisfy` isRight

  it "accepts **kwargs in function definition" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token StarToken "*" (Position 1 7),
        Token StarToken "*" (Position 1 8),
        Token IdentifierToken "kwargs" (Position 1 9),
        Token RParenToken ")" (Position 1 15),
        Token ColonToken ":" (Position 1 16),
        Token NewlineToken "\\n" (Position 1 17),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldSatisfy` isRight

  it "reports ExpectedExpression for unsupported keyword-only separator in function definition" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token StarToken "*" (Position 1 7),
        Token CommaToken "," (Position 1 8),
        Token IdentifierToken "a" (Position 1 10),
        Token RParenToken ")" (Position 1 11),
        Token ColonToken ":" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 7))

  it "reports ExpectedExpression for unsupported positional-only separator in function definition" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token IdentifierToken "a" (Position 1 7),
        Token CommaToken "," (Position 1 8),
        Token SlashToken "/" (Position 1 10),
        Token RParenToken ")" (Position 1 11),
        Token ColonToken ":" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 10))
