module Test.Lexer.ScanTokensBlankLineSpec (spec) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AssignToken, ColonToken, DedentToken, EOFToken, IdentifierToken, IfToken, IndentToken, IntegerToken, NewlineToken, PrintToken))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "scanTokens blank lines" $ do
  it "does not emit a newline token for a blank line between statements" $ do
    scanTokens "x = 1\n\ny = 2\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 6),
          Token IdentifierToken "y" (Position 3 1),
          Token AssignToken "=" (Position 3 3),
          Token IntegerToken "2" (Position 3 5),
          Token NewlineToken "\\n" (Position 3 6),
          Token EOFToken "" (Position 4 1)
        ]

  it "does not emit a newline token for a blank line before an indented body" $ do
    scanTokens "if 1:\n\n    print 1\n"
      `shouldBe` Right
        [ Token IfToken "if" (Position 1 1),
          Token IntegerToken "1" (Position 1 4),
          Token ColonToken ":" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 6),
          Token IndentToken "<INDENT>" (Position 3 1),
          Token PrintToken "print" (Position 3 5),
          Token IntegerToken "1" (Position 3 11),
          Token NewlineToken "\\n" (Position 3 12),
          Token DedentToken "<DEDENT>" (Position 4 1),
          Token EOFToken "" (Position 4 1)
        ]

  it "does not emit a newline token for a whitespace-only line" $ do
    scanTokens "x = 1\n   \ny = 2\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 6),
          Token IdentifierToken "y" (Position 3 1),
          Token AssignToken "=" (Position 3 3),
          Token IntegerToken "2" (Position 3 5),
          Token NewlineToken "\\n" (Position 3 6),
          Token EOFToken "" (Position 4 1)
        ]
