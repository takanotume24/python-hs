module Test.Parser.ParserErrorSpec (spec) where

import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        EOFToken,
        CommaToken,
        ColonToken,
        DefToken,
        ElifToken,
        ElseToken,
        ForToken,
        GlobalToken,
        IdentifierToken,
        IfToken,
        IndentToken,
        InToken,
        IntegerToken,
        LBraceToken,
        LBracketToken,
        LParenToken,
        MinusToken,
        NewlineToken,
        PrintToken,
        GtToken,
        SlashToken,
        RBraceToken,
        StarToken,
        RBracketToken,
        RParenToken,
        WhileToken
      )
  )
import PythonHS.Parser.ParseError (ParseError (ExpectedAssignAfterIdentifier, ExpectedExpression, ExpectedNewlineAfterStatement))
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parse error reporting" $ do
  it "reports ExpectedExpression with position" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 6),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 6))

  it "reports ExpectedAssignAfterIdentifier with position" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 4 3),
        Token NewlineToken "\\n" (Position 4 4),
        Token EOFToken "" (Position 5 1)
      ]
      `shouldBe` Left (ExpectedAssignAfterIdentifier (Position 4 3))

  it "reports ExpectedNewlineAfterStatement with position" $ do
    parseProgram
      [ Token PrintToken "print" (Position 7 1),
        Token IntegerToken "1" (Position 7 7),
        Token PrintToken "print" (Position 7 9),
        Token IntegerToken "2" (Position 7 15),
        Token NewlineToken "\\n" (Position 7 16),
        Token EOFToken "" (Position 8 1)
      ]
      `shouldBe` Left (ExpectedNewlineAfterStatement (Position 7 9))

  it "reports ExpectedExpression for malformed global statement" $ do
    parseProgram
      [ Token GlobalToken "global" (Position 9 1),
        Token NewlineToken "\\n" (Position 9 7),
        Token EOFToken "" (Position 10 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 9 1))

  it "reports ExpectedExpression for if header without suite body" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IntegerToken "1" (Position 1 4),
        Token ColonToken ":" (Position 1 5),
        Token NewlineToken "\\n" (Position 1 6),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 2 1))

  it "reports ExpectedExpression for def header without suite body" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token RParenToken ")" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 2 1))

  it "reports ExpectedExpression for while header without suite body" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 2 1))

  it "reports ExpectedExpression for for header without suite body" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "i" (Position 1 5),
        Token InToken "in" (Position 1 7),
        Token IntegerToken "1" (Position 1 10),
        Token ColonToken ":" (Position 1 11),
        Token NewlineToken "\\n" (Position 1 12),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 2 1))

  it "reports ExpectedExpression for standalone elif header" $ do
    parseProgram
      [ Token ElifToken "elif" (Position 1 1),
        Token IntegerToken "1" (Position 1 6),
        Token ColonToken ":" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 8),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 1))

  it "reports ExpectedExpression for standalone else header" $ do
    parseProgram
      [ Token ElseToken "else" (Position 1 1),
        Token ColonToken ":" (Position 1 5),
        Token NewlineToken "\\n" (Position 1 6),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 1))

  it "reports ExpectedExpression for unexpected leading indentation" $ do
    parseProgram
      [ Token IndentToken "<INDENT>" (Position 1 1),
        Token PrintToken "print" (Position 1 3),
        Token IntegerToken "1" (Position 1 9),
        Token NewlineToken "\\n" (Position 1 10),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 1))

  it "reports ExpectedExpression for if header followed by blank line only" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IntegerToken "1" (Position 1 4),
        Token ColonToken ":" (Position 1 5),
        Token NewlineToken "\\n" (Position 1 6),
        Token NewlineToken "\\n" (Position 2 1),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 2 1))

  it "reports ExpectedExpression for while header followed by blank line only" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token NewlineToken "\\n" (Position 2 1),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 2 1))

  it "reports ExpectedExpression for if header missing colon" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IntegerToken "1" (Position 1 4),
        Token NewlineToken "\\n" (Position 1 5),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 5))

  it "reports ExpectedExpression for while header missing colon" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 8),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 8))

  it "reports ExpectedExpression for for header missing colon" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "i" (Position 1 5),
        Token InToken "in" (Position 1 7),
        Token IntegerToken "1" (Position 1 10),
        Token NewlineToken "\\n" (Position 1 11),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 11))

  it "reports ExpectedExpression for def header missing colon" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token RParenToken ")" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 8),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 8))

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

  it "reports ExpectedExpression for unsupported star expansion in call arguments" $ do
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
      `shouldBe` Left (ExpectedExpression (Position 1 9))

  it "reports ExpectedExpression for unsupported double-star expansion in call arguments" $ do
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
      `shouldBe` Left (ExpectedExpression (Position 1 9))

  it "reports ExpectedExpression for unsupported *args in function definition" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token StarToken "*" (Position 1 7),
        Token IdentifierToken "args" (Position 1 8),
        Token RParenToken ")" (Position 1 12),
        Token ColonToken ":" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 7))

  it "reports ExpectedExpression for unsupported **kwargs in function definition" $ do
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
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 7))

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

  it "reports ExpectedExpression for unsupported type annotation in function parameter" $ do
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
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 8))

  it "reports ExpectedExpression for unsupported return type annotation in function definition" $ do
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
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 9))

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
