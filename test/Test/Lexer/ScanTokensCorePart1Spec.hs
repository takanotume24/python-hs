{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Lexer.ScanTokensCorePart1Spec (spec) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken, AssignToken, AtToken, BreakToken, CaseToken, ClassToken, ColonAssignToken, ColonToken, ContinueToken, DedentToken, DotToken, DoubleSlashAssignToken, DoubleSlashToken, EOFToken, ElifToken, ExceptToken, FalseToken, FinallyToken, FloatToken, ForToken, FromToken, GlobalToken, IdentifierToken, IfToken, ImportToken, InToken, IndentToken, IntegerToken, LParenToken, LambdaToken, MatchToken, MinusAssignToken, NewlineToken, NoneToken, PassToken, PercentAssignToken, PercentToken, PipeToken, PlusAssignToken, PlusToken, PrintToken, RParenToken, RaiseToken, ReturnToken, SlashAssignToken, SlashToken, StarAssignToken, StarToken, StringToken, TrueToken, TryToken, YieldToken))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "scanTokens core tokens - part 1" $ do
  it "tokenizes assignment with newline" $ do
    scanTokens "x = 12\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "12" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes keywords and operators" $ do
    scanTokens "if x + 1\nprint x\n"
      `shouldBe` Right
        [ Token IfToken "if" (Position 1 1),
          Token IdentifierToken "x" (Position 1 4),
          Token PlusToken "+" (Position 1 6),
          Token IntegerToken "1" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token PrintToken "print" (Position 2 1),
          Token IdentifierToken "x" (Position 2 7),
          Token NewlineToken "\\n" (Position 2 8),
          Token EOFToken "" (Position 3 1)
        ]

  it "recognizes at token for decorators" $ do
    scanTokens "@deco\n"
      `shouldBe` Right
        [ Token AtToken "@" (Position 1 1),
          Token IdentifierToken "deco" (Position 1 2),
          Token NewlineToken "\\n" (Position 1 6),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes plus-assign operator" $ do
    scanTokens "x += 1\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token PlusAssignToken "+=" (Position 1 3),
          Token IntegerToken "1" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes minus-assign operator" $ do
    scanTokens "x -= 1\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token MinusAssignToken "-=" (Position 1 3),
          Token IntegerToken "1" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes star-assign operator" $ do
    scanTokens "x *= 3\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token StarAssignToken "*=" (Position 1 3),
          Token IntegerToken "3" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes slash-assign operator" $ do
    scanTokens "x /= 3\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token SlashAssignToken "/=" (Position 1 3),
          Token IntegerToken "3" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes percent-assign operator" $ do
    scanTokens "x %= 3\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token PercentAssignToken "%=" (Position 1 3),
          Token IntegerToken "3" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes double-slash-assign operator" $ do
    scanTokens "x //= 3\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token DoubleSlashAssignToken "//=" (Position 1 3),
          Token IntegerToken "3" (Position 1 7),
          Token NewlineToken "\\n" (Position 1 8),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes elif keyword" $ do
    scanTokens "elif x\n"
      `shouldBe` Right
        [ Token ElifToken "elif" (Position 1 1),
          Token IdentifierToken "x" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes True/False/None keywords" $ do
    scanTokens "print True\nprint False\nprint None\n"
      `shouldBe` Right
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

  it "recognizes double-slash operator" $ do
    scanTokens "print 7 // 2\n"
      `shouldBe` Right
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "7" (Position 1 7),
          Token DoubleSlashToken "//" (Position 1 9),
          Token IntegerToken "2" (Position 1 12),
          Token NewlineToken "\\n" (Position 1 13),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes multiplicative operators" $ do
    scanTokens "print 6 * 2 / 3 // 2 % 2\n"
      `shouldBe` Right
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "6" (Position 1 7),
          Token StarToken "*" (Position 1 9),
          Token IntegerToken "2" (Position 1 11),
          Token SlashToken "/" (Position 1 13),
          Token IntegerToken "3" (Position 1 15),
          Token DoubleSlashToken "//" (Position 1 17),
          Token IntegerToken "2" (Position 1 20),
          Token PercentToken "%" (Position 1 22),
          Token IntegerToken "2" (Position 1 24),
          Token NewlineToken "\\n" (Position 1 25),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes return keyword" $ do
    scanTokens "return x\n"
      `shouldBe` Right
        [ Token ReturnToken "return" (Position 1 1),
          Token IdentifierToken "x" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes yield keyword" $ do
    scanTokens "yield x\n"
      `shouldBe` Right
        [ Token YieldToken "yield" (Position 1 1),
          Token IdentifierToken "x" (Position 1 7),
          Token NewlineToken "\\n" (Position 1 8),
          Token EOFToken "" (Position 2 1)
        ]

  it "skips an inline comment" $ do
    scanTokens "x = 1 # comment\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 16),
          Token EOFToken "" (Position 2 1)
        ]

  it "skips a full-line comment" $ do
    scanTokens "# comment\nx = 1\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 2 1),
          Token AssignToken "=" (Position 2 3),
          Token IntegerToken "1" (Position 2 5),
          Token NewlineToken "\\n" (Position 2 6),
          Token EOFToken "" (Position 3 1)
        ]
