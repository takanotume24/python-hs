{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Lexer.ScanTokensCorePart2Spec (spec) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType (TokenType (AsToken, AssignToken, AtToken, BreakToken, CaseToken, ClassToken, ColonAssignToken, ColonToken, ContinueToken, DedentToken, DotToken, DoubleSlashAssignToken, DoubleSlashToken, EOFToken, ElifToken, ExceptToken, FalseToken, FinallyToken, FloatToken, ForToken, FromToken, GlobalToken, IdentifierToken, IfToken, ImportToken, InToken, IndentToken, IntegerToken, LParenToken, LambdaToken, MatchToken, MinusAssignToken, NewlineToken, NoneToken, PassToken, PercentAssignToken, PercentToken, PipeToken, PlusAssignToken, PlusToken, PrintToken, RParenToken, RaiseToken, ReturnToken, SlashAssignToken, SlashToken, StarAssignToken, StarToken, StringToken, TrueToken, TryToken, YieldToken))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "scanTokens core tokens - part 2" $ do
  it "recognizes for/in/break/continue keywords" $ do
    scanTokens "for i in x\nbreak\ncontinue\n"
      `shouldBe` Right
        [ Token ForToken "for" (Position 1 1),
          Token IdentifierToken "i" (Position 1 5),
          Token InToken "in" (Position 1 7),
          Token IdentifierToken "x" (Position 1 10),
          Token NewlineToken "\\n" (Position 1 11),
          Token BreakToken "break" (Position 2 1),
          Token NewlineToken "\\n" (Position 2 6),
          Token ContinueToken "continue" (Position 3 1),
          Token NewlineToken "\\n" (Position 3 9),
          Token EOFToken "" (Position 4 1)
        ]

  it "recognizes global keyword" $ do
    scanTokens "global x\n"
      `shouldBe` Right
        [ Token GlobalToken "global" (Position 1 1),
          Token IdentifierToken "x" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes pass keyword" $ do
    scanTokens "pass\n"
      `shouldBe` Right
        [ Token PassToken "pass" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 5),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes import keyword" $ do
    scanTokens "import math\n"
      `shouldBe` Right
        [ Token ImportToken "import" (Position 1 1),
          Token IdentifierToken "math" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 12),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes class keyword" $ do
    scanTokens "class A:\n"
      `shouldBe` Right
        [ Token ClassToken "class" (Position 1 1),
          Token IdentifierToken "A" (Position 1 7),
          Token ColonToken ":" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes lambda keyword" $ do
    scanTokens "f = lambda x: x\n"
      `shouldBe` Right
        [ Token IdentifierToken "f" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token LambdaToken "lambda" (Position 1 5),
          Token IdentifierToken "x" (Position 1 12),
          Token ColonToken ":" (Position 1 13),
          Token IdentifierToken "x" (Position 1 15),
          Token NewlineToken "\\n" (Position 1 16),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes walrus operator token" $ do
    scanTokens "print (y := 1)\n"
      `shouldBe` Right
        [ Token PrintToken "print" (Position 1 1),
          Token LParenToken "(" (Position 1 7),
          Token IdentifierToken "y" (Position 1 8),
          Token ColonAssignToken ":=" (Position 1 10),
          Token IntegerToken "1" (Position 1 13),
          Token RParenToken ")" (Position 1 14),
          Token NewlineToken "\\n" (Position 1 15),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes try/except/finally/raise keywords" $ do
    scanTokens "try:\nexcept:\nfinally:\nraise \"x\"\n"
      `shouldBe` Right
        [ Token TryToken "try" (Position 1 1),
          Token ColonToken ":" (Position 1 4),
          Token NewlineToken "\\n" (Position 1 5),
          Token ExceptToken "except" (Position 2 1),
          Token ColonToken ":" (Position 2 7),
          Token NewlineToken "\\n" (Position 2 8),
          Token FinallyToken "finally" (Position 3 1),
          Token ColonToken ":" (Position 3 8),
          Token NewlineToken "\\n" (Position 3 9),
          Token RaiseToken "raise" (Position 4 1),
          Token StringToken "x" (Position 4 7),
          Token NewlineToken "\\n" (Position 4 10),
          Token EOFToken "" (Position 5 1)
        ]

  it "recognizes match/case and pattern tokens" $ do
    scanTokens "match x:\n  case 1 | 2:\n    print 1\n"
      `shouldBe` Right
        [ Token MatchToken "match" (Position 1 1),
          Token IdentifierToken "x" (Position 1 7),
          Token ColonToken ":" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token IndentToken "<INDENT>" (Position 2 1),
          Token CaseToken "case" (Position 2 3),
          Token IntegerToken "1" (Position 2 8),
          Token PipeToken "|" (Position 2 10),
          Token IntegerToken "2" (Position 2 12),
          Token ColonToken ":" (Position 2 13),
          Token NewlineToken "\\n" (Position 2 14),
          Token IndentToken "<INDENT>" (Position 3 1),
          Token PrintToken "print" (Position 3 5),
          Token IntegerToken "1" (Position 3 11),
          Token NewlineToken "\\n" (Position 3 12),
          Token DedentToken "<DEDENT>" (Position 4 1),
          Token DedentToken "<DEDENT>" (Position 4 1),
          Token EOFToken "" (Position 4 1)
        ]

  it "recognizes from/as keywords for import variants" $ do
    scanTokens "from pkg.mod import sqrt as s\n"
      `shouldBe` Right
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

  it "recognizes dot token for method-call syntax" $ do
    scanTokens "x.append(1)\n"
      `shouldBe` Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token DotToken "." (Position 1 2),
          Token IdentifierToken "append" (Position 1 3),
          Token LParenToken "(" (Position 1 9),
          Token IntegerToken "1" (Position 1 10),
          Token RParenToken ")" (Position 1 11),
          Token NewlineToken "\\n" (Position 1 12),
          Token EOFToken "" (Position 2 1)
        ]

  it "does not lex integer method call as float literal" $ do
    scanTokens "1.append(2)\n"
      `shouldBe` Right
        [ Token IntegerToken "1" (Position 1 1),
          Token DotToken "." (Position 1 2),
          Token IdentifierToken "append" (Position 1 3),
          Token LParenToken "(" (Position 1 9),
          Token IntegerToken "2" (Position 1 10),
          Token RParenToken ")" (Position 1 11),
          Token NewlineToken "\\n" (Position 1 12),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes float literals including scientific notation" $ do
    scanTokens "print 1.23\nprint 1.\nprint .5\nprint 1e3\nprint 1.2e-3\n"
      `shouldBe` Right
        [ Token PrintToken "print" (Position 1 1),
          Token FloatToken "1.23" (Position 1 7),
          Token NewlineToken "\\n" (Position 1 11),
          Token PrintToken "print" (Position 2 1),
          Token FloatToken "1." (Position 2 7),
          Token NewlineToken "\\n" (Position 2 9),
          Token PrintToken "print" (Position 3 1),
          Token FloatToken ".5" (Position 3 7),
          Token NewlineToken "\\n" (Position 3 9),
          Token PrintToken "print" (Position 4 1),
          Token FloatToken "1e3" (Position 4 7),
          Token NewlineToken "\\n" (Position 4 10),
          Token PrintToken "print" (Position 5 1),
          Token FloatToken "1.2e-3" (Position 5 7),
          Token NewlineToken "\\n" (Position 5 13),
          Token EOFToken "" (Position 6 1)
        ]
