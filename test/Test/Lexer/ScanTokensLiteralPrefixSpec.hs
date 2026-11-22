module Test.Lexer.ScanTokensLiteralPrefixSpec (spec) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        BytesToken,
        CommaToken,
        EOFToken,
        IdentifierToken,
        IntegerToken,
        LBracketToken,
        NewlineToken,
        PlusToken,
        RBracketToken,
        StringToken
      ),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "scanTokens literal prefixes" $ do
  it "tokenizes a raw string without decoding escapes" $ do
    scanTokens "r\"\\n\"\n"
      `shouldBe` Right
        [ Token StringToken "\\n" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 6),
          Token EOFToken "" (Position 2 1)
        ]

  it "tokenizes a bytes literal as a distinct token" $ do
    scanTokens "b\"abc\"\n"
      `shouldBe` Right
        [ Token BytesToken "abc" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "tokenizes a raw bytes literal" $ do
    scanTokens "rb\"a\\n\"\n"
      `shouldBe` Right
        [ Token BytesToken "a\\n" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 8),
          Token EOFToken "" (Position 2 1)
        ]

  it "treats a backslash-newline as line continuation" $ do
    scanTokens "x = 1 + \\\n    2\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token PlusToken "+" (Position 1 7),
          Token IntegerToken "2" (Position 2 5),
          Token NewlineToken "\\n" (Position 2 6),
          Token EOFToken "" (Position 3 1)
        ]

  it "suppresses newlines inside brackets" $ do
    scanTokens "[1,\n 2]\n"
      `shouldBe` Right
        [ Token LBracketToken "[" (Position 1 1),
          Token IntegerToken "1" (Position 1 2),
          Token CommaToken "," (Position 1 3),
          Token IntegerToken "2" (Position 2 2),
          Token RBracketToken "]" (Position 2 3),
          Token NewlineToken "\\n" (Position 2 4),
          Token EOFToken "" (Position 3 1)
        ]

  it "turns a semicolon into a statement separator" $ do
    scanTokens "x = 1; y = 2\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token NewlineToken ";" (Position 1 6),
          Token IdentifierToken "y" (Position 1 8),
          Token AssignToken "=" (Position 1 10),
          Token IntegerToken "2" (Position 1 12),
          Token NewlineToken "\\n" (Position 1 13),
          Token EOFToken "" (Position 2 1)
        ]

  it "ignores a trailing semicolon" $ do
    scanTokens "x = 1;\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]
