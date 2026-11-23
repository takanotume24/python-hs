module Test.Lexer.ScanTokensTripleQuoteSpec (spec) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (EOFToken, IntegerToken, NewlineToken, PrintToken, StringToken))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "scanTokens triple-quoted strings" $ do
  it "tokenizes a single-line triple-quoted string" $ do
    scanTokens "\"\"\"hello\"\"\"\n"
      `shouldBe` Right
        [ Token StringToken "hello" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 12),
          Token EOFToken "" (Position 2 1)
        ]

  it "tokenizes a multi-line triple-quoted string and keeps line numbers" $ do
    scanTokens "\"\"\"a\nb\"\"\"\nprint 1\n"
      `shouldBe` Right
        [ Token StringToken "a\nb" (Position 1 1),
          Token NewlineToken "\\n" (Position 2 5),
          Token PrintToken "print" (Position 3 1),
          Token IntegerToken "1" (Position 3 7),
          Token NewlineToken "\\n" (Position 3 8),
          Token EOFToken "" (Position 4 1)
        ]

  it "tokenizes a raw triple-quoted string without decoding escapes" $ do
    scanTokens "r'''a\\nb'''\n"
      `shouldBe` Right
        [ Token StringToken "a\\nb" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 11),
          Token EOFToken "" (Position 2 1)
        ]
