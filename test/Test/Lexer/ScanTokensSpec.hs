module Test.Lexer.ScanTokensSpec (spec) where

import PythonHS.Lexer.LexerError (LexerError (UnexpectedCharacter))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        PlusAssignToken,
        MinusAssignToken,
        StarAssignToken,
        SlashAssignToken,
        PercentAssignToken,
        DoubleSlashAssignToken,
        ColonToken,
        CommaToken,
        EOFToken,
        IdentifierToken,
        IfToken,
        ElifToken,
        TrueToken,
        FalseToken,
        NoneToken,
        ForToken,
        InToken,
        IntegerToken,
        StringToken,
        NewlineToken,
        PrintToken,
        PlusToken,
        StarToken,
        SlashToken,
        PercentToken,
        ReturnToken,
        BreakToken,
        ContinueToken,
        GlobalToken,
        PassToken,
        IndentToken,
        DedentToken,
        EqToken,
        NotEqToken,
        LtToken,
        GtToken,
        LteToken,
        GteToken,
        LBracketToken,
        RBracketToken,
        LBraceToken,
        RBraceToken,
        AndToken,
        OrToken,
        NotToken
      )
  )
import PythonHS.Lexer.ScanTokens (scanTokens)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "scanTokens" $ do
  it "tokenizes assignment with newline" $ do
    scanTokens "x = 12\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "12" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes keywords and operators" $ do
    scanTokens "if x + 1\nprint x\n" `shouldBe`
      Right
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

  it "recognizes plus-assign operator" $ do
    scanTokens "x += 1\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token PlusAssignToken "+=" (Position 1 3),
          Token IntegerToken "1" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes minus-assign operator" $ do
    scanTokens "x -= 1\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token MinusAssignToken "-=" (Position 1 3),
          Token IntegerToken "1" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes star-assign operator" $ do
    scanTokens "x *= 3\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token StarAssignToken "*=" (Position 1 3),
          Token IntegerToken "3" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes slash-assign operator" $ do
    scanTokens "x /= 3\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token SlashAssignToken "/=" (Position 1 3),
          Token IntegerToken "3" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes percent-assign operator" $ do
    scanTokens "x %= 3\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token PercentAssignToken "%=" (Position 1 3),
          Token IntegerToken "3" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes double-slash-assign operator" $ do
    scanTokens "x //= 3\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token DoubleSlashAssignToken "//=" (Position 1 3),
          Token IntegerToken "3" (Position 1 7),
          Token NewlineToken "\\n" (Position 1 8),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes elif keyword" $ do
    scanTokens "elif x\n" `shouldBe`
      Right
        [ Token ElifToken "elif" (Position 1 1),
          Token IdentifierToken "x" (Position 1 6),
          Token NewlineToken "\\n" (Position 1 7),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes True/False/None keywords" $ do
    scanTokens "print True\nprint False\nprint None\n" `shouldBe`
      Right
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

  it "recognizes multiplicative operators" $ do
    scanTokens "print 6 * 2 / 3 % 2\n" `shouldBe`
      Right
        [ Token PrintToken "print" (Position 1 1),
          Token IntegerToken "6" (Position 1 7),
          Token StarToken "*" (Position 1 9),
          Token IntegerToken "2" (Position 1 11),
          Token SlashToken "/" (Position 1 13),
          Token IntegerToken "3" (Position 1 15),
          Token PercentToken "%" (Position 1 17),
          Token IntegerToken "2" (Position 1 19),
          Token NewlineToken "\\n" (Position 1 20),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes return keyword" $ do
    scanTokens "return x\n" `shouldBe`
      Right
        [ Token ReturnToken "return" (Position 1 1),
          Token IdentifierToken "x" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes for/in/break/continue keywords" $ do
    scanTokens "for i in x\nbreak\ncontinue\n" `shouldBe`
      Right
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
    scanTokens "global x\n" `shouldBe`
      Right
        [ Token GlobalToken "global" (Position 1 1),
          Token IdentifierToken "x" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes pass keyword" $ do
    scanTokens "pass\n" `shouldBe`
      Right
        [ Token PassToken "pass" (Position 1 1),
          Token NewlineToken "\\n" (Position 1 5),
          Token EOFToken "" (Position 2 1)
        ]

  it "emits INDENT/DEDENT for leading spaces" $ do
    scanTokens "if 1:\n  print 1\nprint 2\n" `shouldBe`
      Right
        [ Token IfToken "if" (Position 1 1),
          Token IntegerToken "1" (Position 1 4),
          Token ColonToken ":" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 6),
          Token IndentToken "<INDENT>" (Position 2 1),
          Token PrintToken "print" (Position 2 3),
          Token IntegerToken "1" (Position 2 9),
          Token NewlineToken "\\n" (Position 2 10),
          Token DedentToken "<DEDENT>" (Position 3 1),
          Token PrintToken "print" (Position 3 1),
          Token IntegerToken "2" (Position 3 7),
          Token NewlineToken "\\n" (Position 3 8),
          Token EOFToken "" (Position 4 1)
        ]

  it "recognizes comparison operators" $ do
    scanTokens "a == b != c < 3 <= 4 > 2 >= 1\n" `shouldBe`
      Right
        [ Token IdentifierToken "a" (Position 1 1),
          Token EqToken "==" (Position 1 3),
          Token IdentifierToken "b" (Position 1 6),
          Token NotEqToken "!=" (Position 1 8),
          Token IdentifierToken "c" (Position 1 11),
          Token LtToken "<" (Position 1 13),
          Token IntegerToken "3" (Position 1 15),
          Token LteToken "<=" (Position 1 17),
          Token IntegerToken "4" (Position 1 20),
          Token GtToken ">" (Position 1 22),
          Token IntegerToken "2" (Position 1 24),
          Token GteToken ">=" (Position 1 26),
          Token IntegerToken "1" (Position 1 29),
          Token NewlineToken "\\n" (Position 1 30),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes logical operators" $ do
    scanTokens "x and y or not z\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AndToken "and" (Position 1 3),
          Token IdentifierToken "y" (Position 1 7),
          Token OrToken "or" (Position 1 9),
          Token NotToken "not" (Position 1 12),
          Token IdentifierToken "z" (Position 1 16),
          Token NewlineToken "\\n" (Position 1 17),
          Token EOFToken "" (Position 2 1)
        ]

  it "returns error on unexpected characters" $ do
    scanTokens "x @ 1\n" `shouldBe` Left (UnexpectedCharacter '@')

  it "accepts tab-indented input as leading whitespace" $ do
    scanTokens "\tprint 1\n" `shouldBe`
      Right
        [ Token PrintToken "print" (Position 1 2),
          Token IntegerToken "1" (Position 1 8),
          Token NewlineToken "\\n" (Position 1 9),
          Token EOFToken "" (Position 2 1)
        ]

  it "accepts tabs between tokens" $ do
    scanTokens "x\t=\t1\n" `shouldBe`
      Right
        [ Token IdentifierToken "x" (Position 1 1),
          Token AssignToken "=" (Position 1 3),
          Token IntegerToken "1" (Position 1 5),
          Token NewlineToken "\\n" (Position 1 6),
          Token EOFToken "" (Position 2 1)
        ]

  it "accepts tab between keyword and identifier" $ do
    scanTokens "print\tx\n" `shouldBe`
      Right
        [ Token PrintToken "print" (Position 1 1),
          Token IdentifierToken "x" (Position 1 7),
          Token NewlineToken "\\n" (Position 1 8),
          Token EOFToken "" (Position 2 1)
        ]

  it "returns error on inconsistent dedent indentation" $ do
    scanTokens "if 1:\n  print 1\n print 2\n" `shouldBe` Left (UnexpectedCharacter ' ')

  it "recognizes double-quoted string literals" $ do
    scanTokens "print \"hello\"\n" `shouldBe`
      Right
        [ Token PrintToken "print" (Position 1 1),
          Token StringToken "hello" (Position 1 7),
          Token NewlineToken "\\n" (Position 1 14),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes list literal brackets" $ do
    scanTokens "print [1, 2]\n" `shouldBe`
      Right
        [ Token PrintToken "print" (Position 1 1),
          Token LBracketToken "[" (Position 1 7),
          Token IntegerToken "1" (Position 1 8),
          Token CommaToken "," (Position 1 9),
          Token IntegerToken "2" (Position 1 11),
          Token RBracketToken "]" (Position 1 12),
          Token NewlineToken "\\n" (Position 1 13),
          Token EOFToken "" (Position 2 1)
        ]

  it "recognizes dictionary literal braces" $ do
    scanTokens "print {1: 2}\n" `shouldBe`
      Right
        [ Token PrintToken "print" (Position 1 1),
          Token LBraceToken "{" (Position 1 7),
          Token IntegerToken "1" (Position 1 8),
          Token ColonToken ":" (Position 1 9),
          Token IntegerToken "2" (Position 1 11),
          Token RBraceToken "}" (Position 1 12),
          Token NewlineToken "\\n" (Position 1 13),
          Token EOFToken "" (Position 2 1)
        ]

