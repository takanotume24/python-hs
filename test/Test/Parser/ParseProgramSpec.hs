module Test.Parser.ParseProgramSpec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FromImportStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
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
        DotToken,
        DefToken,
        EOFToken,
        ElseToken,
        ElifToken,
        AsToken,
        TrueToken,
        FalseToken,
        FloatToken,
        FromToken,
        NoneToken,
        ForToken,
        ImportToken,
        IdentifierToken,
        InToken,
        IntegerToken,
        StringToken,
        LParenToken,
        NewlineToken,
        PlusToken,
        MinusToken,
        StarToken,
        SlashToken,
        PercentToken,
        DoubleSlashToken,
        PrintToken,
        RParenToken,
        ReturnToken,
        BreakToken,
        ContinueToken,
        GlobalToken,
        PassToken,
        IndentToken,
        DedentToken,
        EqToken,
        AndToken,
        OrToken,
        NotToken,
        IfToken,
        WhileToken,
        LBracketToken,
        RBracketToken,
        LBraceToken,
        RBraceToken
      )
  )
import PythonHS.Parser.ParseError (ParseError (ExpectedExpression))
import PythonHS.Parser.ParseProgram (parseProgram)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "parseProgram" $ do
  it "parses import statement" $ do
    parseProgram
      [ Token ImportToken "import" (Position 1 1),
        Token IdentifierToken "math" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 12),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ ImportStmt [(["math"], Nothing)] (Position 1 1)
            ]
        )

  it "parses import statement with dotted module and alias" $ do
    parseProgram
      [ Token ImportToken "import" (Position 1 1),
        Token IdentifierToken "pkg" (Position 1 8),
        Token DotToken "." (Position 1 11),
        Token IdentifierToken "mod" (Position 1 12),
        Token AsToken "as" (Position 1 16),
        Token IdentifierToken "m" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ ImportStmt [(["pkg", "mod"], Just "m")] (Position 1 1)
            ]
        )

  it "parses from-import statement with alias" $ do
    parseProgram
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
      `shouldBe` Right
        ( Program
            [ FromImportStmt ["pkg", "mod"] [("sqrt", Just "s")] (Position 1 1)
            ]
        )

  it "parses float literal expressions" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token FloatToken "1.5" (Position 1 7),
        Token PlusToken "+" (Position 1 11),
        Token IntegerToken "2" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr AddOperator (FloatExpr 1.5 (Position 1 7)) (IntegerExpr 2 (Position 1 13)) (Position 1 11))
                (Position 1 1)
            ]
        )

  it "parses assignment and print statements" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token AssignToken "=" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token PlusToken "+" (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right
        ( Program
            [ AssignStmt "x" (IntegerExpr 1 (Position 1 1)) (Position 1 1),
              PrintStmt (BinaryExpr AddOperator (IdentifierExpr "x" (Position 1 1)) (IntegerExpr 2 (Position 1 1)) (Position 1 1)) (Position 1 1)
            ]
        )

  it "parses plus-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token PlusAssignToken "+=" (Position 1 3),
        Token IntegerToken "2" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ AddAssignStmt "x" (IntegerExpr 2 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses minus-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token MinusAssignToken "-=" (Position 1 3),
        Token IntegerToken "2" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ SubAssignStmt "x" (IntegerExpr 2 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses star-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token StarAssignToken "*=" (Position 1 3),
        Token IntegerToken "3" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ MulAssignStmt "x" (IntegerExpr 3 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses slash-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token SlashAssignToken "/=" (Position 1 3),
        Token IntegerToken "3" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ DivAssignStmt "x" (IntegerExpr 3 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses percent-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token PercentAssignToken "%=" (Position 1 3),
        Token IntegerToken "3" (Position 1 6),
        Token NewlineToken "\\n" (Position 1 7),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ ModAssignStmt "x" (IntegerExpr 3 (Position 1 6)) (Position 1 1)
            ]
        )

  it "parses double-slash-assign statement" $ do
    parseProgram
      [ Token IdentifierToken "x" (Position 1 1),
        Token DoubleSlashAssignToken "//=" (Position 1 3),
        Token IntegerToken "3" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 8),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FloorDivAssignStmt "x" (IntegerExpr 3 (Position 1 7)) (Position 1 1)
            ]
        )

  it "returns parse error for invalid expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 6),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Left (ExpectedExpression (Position 1 6))

  it "parses an if statement without else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [IfStmt (IdentifierExpr "x" (Position 1 1)) [PrintStmt (IdentifierExpr "x" (Position 1 1)) (Position 1 1)] Nothing (Position 1 1)])

  it "parses an if statement with else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token ElseToken "else" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [IfStmt (IdentifierExpr "x" (Position 1 1)) [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Just [PrintStmt (IdentifierExpr "b" (Position 1 1)) (Position 1 1)]) (Position 1 1)])

  it "parses an if statement with newline before else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token ElseToken "else" (Position 2 1),
        Token ColonToken ":" (Position 2 1),
        Token PrintToken "print" (Position 2 1),
        Token IdentifierToken "b" (Position 2 1),
        Token NewlineToken "\\n" (Position 2 1),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [IfStmt (IdentifierExpr "x" (Position 1 1)) [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Just [PrintStmt (IdentifierExpr "b" (Position 2 1)) (Position 2 1)]) (Position 1 1)])

  it "parses an if statement with elif and else" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "x" (Position 1 4),
        Token ColonToken ":" (Position 1 5),
        Token PrintToken "print" (Position 2 1),
        Token IntegerToken "1" (Position 2 7),
        Token NewlineToken "\\n" (Position 2 8),
        Token ElifToken "elif" (Position 3 1),
        Token IdentifierToken "y" (Position 3 6),
        Token ColonToken ":" (Position 3 7),
        Token PrintToken "print" (Position 4 1),
        Token IntegerToken "2" (Position 4 7),
        Token NewlineToken "\\n" (Position 4 8),
        Token ElseToken "else" (Position 5 1),
        Token ColonToken ":" (Position 5 5),
        Token PrintToken "print" (Position 6 1),
        Token IntegerToken "3" (Position 6 7),
        Token NewlineToken "\\n" (Position 6 8),
        Token EOFToken "" (Position 7 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "x" (Position 1 4))
                [PrintStmt (IntegerExpr 1 (Position 2 7)) (Position 2 1)]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "y" (Position 3 6))
                        [PrintStmt (IntegerExpr 2 (Position 4 7)) (Position 4 1)]
                        (Just [PrintStmt (IntegerExpr 3 (Position 6 7)) (Position 6 1)])
                        (Position 3 1)
                    ]
                )
                (Position 1 1)
            ]
        )

  it "parses indented if/elif/else suites with multiple statements" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 4),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token IdentifierToken "x" (Position 3 3),
        Token AssignToken "=" (Position 3 5),
        Token IntegerToken "1" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token ElifToken "elif" (Position 4 1),
        Token IdentifierToken "alt" (Position 4 6),
        Token ColonToken ":" (Position 4 9),
        Token NewlineToken "\\n" (Position 4 10),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PassToken "pass" (Position 5 3),
        Token NewlineToken "\\n" (Position 5 7),
        Token IdentifierToken "x" (Position 6 3),
        Token AssignToken "=" (Position 6 5),
        Token IntegerToken "2" (Position 6 7),
        Token NewlineToken "\\n" (Position 6 8),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token ElseToken "else" (Position 7 1),
        Token ColonToken ":" (Position 7 5),
        Token NewlineToken "\\n" (Position 7 6),
        Token IndentToken "<INDENT>" (Position 8 1),
        Token PassToken "pass" (Position 8 3),
        Token NewlineToken "\\n" (Position 8 7),
        Token IdentifierToken "x" (Position 9 3),
        Token AssignToken "=" (Position 9 5),
        Token IntegerToken "3" (Position 9 7),
        Token NewlineToken "\\n" (Position 9 8),
        Token DedentToken "<DEDENT>" (Position 10 1),
        Token EOFToken "" (Position 10 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [ PassStmt (Position 2 3),
                  AssignStmt "x" (IntegerExpr 1 (Position 3 7)) (Position 3 3)
                ]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "alt" (Position 4 6))
                        [ PassStmt (Position 5 3),
                          AssignStmt "x" (IntegerExpr 2 (Position 6 7)) (Position 6 3)
                        ]
                        ( Just
                            [ PassStmt (Position 8 3),
                              AssignStmt "x" (IntegerExpr 3 (Position 9 7)) (Position 9 3)
                            ]
                        )
                        (Position 4 1)
                    ]
                )
                (Position 1 1)
            ]
        )

  it "parses a statement after indented if/elif/else suites" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 4),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ElifToken "elif" (Position 3 1),
        Token IdentifierToken "alt" (Position 3 6),
        Token ColonToken ":" (Position 3 9),
        Token NewlineToken "\\n" (Position 3 10),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token PassToken "pass" (Position 4 3),
        Token NewlineToken "\\n" (Position 4 7),
        Token DedentToken "<DEDENT>" (Position 5 1),
        Token ElseToken "else" (Position 5 1),
        Token ColonToken ":" (Position 5 5),
        Token NewlineToken "\\n" (Position 5 6),
        Token IndentToken "<INDENT>" (Position 6 1),
        Token PassToken "pass" (Position 6 3),
        Token NewlineToken "\\n" (Position 6 7),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 1),
        Token IntegerToken "9" (Position 7 7),
        Token NewlineToken "\\n" (Position 7 8),
        Token EOFToken "" (Position 8 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [PassStmt (Position 2 3)]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "alt" (Position 3 6))
                        [PassStmt (Position 4 3)]
                        (Just [PassStmt (Position 6 3)])
                        (Position 3 1)
                    ]
                )
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 7 7)) (Position 7 1)
            ]
        )

  it "parses nested conditional suites inside elif branch and continues" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 4),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token DedentToken "<DEDENT>" (Position 3 1),
        Token ElifToken "elif" (Position 3 1),
        Token IdentifierToken "alt" (Position 3 6),
        Token ColonToken ":" (Position 3 9),
        Token NewlineToken "\\n" (Position 3 10),
        Token IndentToken "<INDENT>" (Position 4 1),
        Token IfToken "if" (Position 4 3),
        Token IdentifierToken "inner" (Position 4 6),
        Token ColonToken ":" (Position 4 11),
        Token NewlineToken "\\n" (Position 4 12),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PrintToken "print" (Position 5 5),
        Token IntegerToken "1" (Position 5 11),
        Token NewlineToken "\\n" (Position 5 12),
        Token DedentToken "<DEDENT>" (Position 6 1),
        Token ElseToken "else" (Position 6 3),
        Token ColonToken ":" (Position 6 7),
        Token NewlineToken "\\n" (Position 6 8),
        Token IndentToken "<INDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 5),
        Token IntegerToken "2" (Position 7 11),
        Token NewlineToken "\\n" (Position 7 12),
        Token DedentToken "<DEDENT>" (Position 8 1),
        Token DedentToken "<DEDENT>" (Position 8 1),
        Token ElseToken "else" (Position 8 1),
        Token ColonToken ":" (Position 8 5),
        Token NewlineToken "\\n" (Position 8 6),
        Token IndentToken "<INDENT>" (Position 9 1),
        Token PrintToken "print" (Position 9 3),
        Token IntegerToken "3" (Position 9 9),
        Token NewlineToken "\\n" (Position 9 10),
        Token DedentToken "<DEDENT>" (Position 10 1),
        Token PrintToken "print" (Position 10 1),
        Token IntegerToken "9" (Position 10 7),
        Token NewlineToken "\\n" (Position 10 8),
        Token EOFToken "" (Position 11 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [PassStmt (Position 2 3)]
                ( Just
                    [ IfStmt
                        (IdentifierExpr "alt" (Position 3 6))
                        [ IfStmt
                            (IdentifierExpr "inner" (Position 4 6))
                            [PrintStmt (IntegerExpr 1 (Position 5 11)) (Position 5 5)]
                            (Just [PrintStmt (IntegerExpr 2 (Position 7 11)) (Position 7 5)])
                            (Position 4 3)
                        ]
                        (Just [PrintStmt (IntegerExpr 3 (Position 9 9)) (Position 9 3)])
                        (Position 3 1)
                    ]
                )
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 10 7)) (Position 10 1)
            ]
        )

  it "parses an indented if suite with multiple statements" $ do
    parseProgram
      [ Token IfToken "if" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 4),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token IdentifierToken "x" (Position 3 3),
        Token AssignToken "=" (Position 3 5),
        Token IntegerToken "1" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ IfStmt
                (IdentifierExpr "cond" (Position 1 4))
                [ PassStmt (Position 2 3),
                  AssignStmt "x" (IntegerExpr 1 (Position 3 7)) (Position 3 3)
                ]
                Nothing
                (Position 1 1)
            ]
        )

  it "parses a while statement" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [WhileStmt (IdentifierExpr "cond" (Position 1 1)) [PrintStmt (IdentifierExpr "cond" (Position 1 1)) (Position 1 1)] (Position 1 1)])

  it "parses an indented while suite with multiple statements" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 7),
        Token ColonToken ":" (Position 1 11),
        Token NewlineToken "\\n" (Position 1 12),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token IdentifierToken "x" (Position 3 3),
        Token AssignToken "=" (Position 3 5),
        Token IntegerToken "1" (Position 3 7),
        Token NewlineToken "\\n" (Position 3 8),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ WhileStmt
                (IdentifierExpr "cond" (Position 1 7))
                [ PassStmt (Position 2 3),
                  AssignStmt "x" (IntegerExpr 1 (Position 3 7)) (Position 3 3)
                ]
                (Position 1 1)
            ]
        )

  it "parses nested conditional inside while suite and continues" $ do
    parseProgram
      [ Token WhileToken "while" (Position 1 1),
        Token IdentifierToken "cond" (Position 1 7),
        Token ColonToken ":" (Position 1 11),
        Token NewlineToken "\\n" (Position 1 12),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token IfToken "if" (Position 2 3),
        Token IdentifierToken "inner" (Position 2 6),
        Token ColonToken ":" (Position 2 11),
        Token NewlineToken "\\n" (Position 2 12),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PrintToken "print" (Position 3 5),
        Token IntegerToken "1" (Position 3 11),
        Token NewlineToken "\\n" (Position 3 12),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token ElseToken "else" (Position 4 3),
        Token ColonToken ":" (Position 4 7),
        Token NewlineToken "\\n" (Position 4 8),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PrintToken "print" (Position 5 5),
        Token IntegerToken "2" (Position 5 11),
        Token NewlineToken "\\n" (Position 5 12),
        Token DedentToken "<DEDENT>" (Position 6 1),
        Token PassToken "pass" (Position 6 3),
        Token NewlineToken "\\n" (Position 6 7),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 1),
        Token IntegerToken "9" (Position 7 7),
        Token NewlineToken "\\n" (Position 7 8),
        Token EOFToken "" (Position 8 1)
      ]
      `shouldBe` Right
        ( Program
            [ WhileStmt
                (IdentifierExpr "cond" (Position 1 7))
                [ IfStmt
                    (IdentifierExpr "inner" (Position 2 6))
                    [PrintStmt (IntegerExpr 1 (Position 3 11)) (Position 3 5)]
                    (Just [PrintStmt (IntegerExpr 2 (Position 5 11)) (Position 5 5)])
                    (Position 2 3),
                  PassStmt (Position 6 3)
                ]
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 7 7)) (Position 7 1)
            ]
        )

  it "parses binary subtraction with precedence" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "5" (Position 1 7),
        Token MinusToken "-" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token StarToken "*" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( BinaryExpr
                    SubtractOperator
                    (IntegerExpr 5 (Position 1 7))
                    (BinaryExpr MultiplyOperator (IntegerExpr 2 (Position 1 11)) (IntegerExpr 3 (Position 1 15)) (Position 1 13))
                    (Position 1 9)
                )
                (Position 1 1)
            ]
        )

  it "parses floor-division operator" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "7" (Position 1 7),
        Token DoubleSlashToken "//" (Position 1 9),
        Token IntegerToken "2" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr FloorDivideOperator (IntegerExpr 7 (Position 1 7)) (IntegerExpr 2 (Position 1 12)) (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses a global statement" $ do
    parseProgram
      [ Token GlobalToken "global" (Position 1 1),
        Token IdentifierToken "x" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [GlobalStmt "x" (Position 1 1)])

  it "parses a pass statement" $ do
    parseProgram
      [ Token PassToken "pass" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 5),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PassStmt (Position 1 1)])

  it "parses function definition and call" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefStmt "add" ["a", "b"] [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Position 1 1),
              PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 1 1), IntegerExpr 2 (Position 1 1)] (Position 1 1)) (Position 1 1)
            ]
        )

  it "parses function definition and call with trailing commas" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "add" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token CommaToken "," (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefStmt "add" ["a", "b"] [PrintStmt (IdentifierExpr "a" (Position 1 1)) (Position 1 1)] (Position 1 1),
              PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 1 1), IntegerExpr 2 (Position 1 1)] (Position 1 1)) (Position 1 1)
            ]
        )

  it "parses function definition with default parameter" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 14),
        Token IntegerToken "2" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token ColonToken ":" (Position 1 18),
        Token PrintToken "print" (Position 1 20),
        Token IdentifierToken "a" (Position 1 26),
        Token NewlineToken "\\n" (Position 1 27),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefDefaultsStmt
                "add"
                ["a", "b"]
                [("b", IntegerExpr 2 (Position 1 16))]
                [PrintStmt (IdentifierExpr "a" (Position 1 26)) (Position 1 20)]
                (Position 1 1)
            ]
        )

  it "parses function definition with default parameter and trailing comma" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "add" (Position 1 5),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 14),
        Token IntegerToken "2" (Position 1 16),
        Token CommaToken "," (Position 1 17),
        Token RParenToken ")" (Position 1 18),
        Token ColonToken ":" (Position 1 19),
        Token PrintToken "print" (Position 1 21),
        Token IdentifierToken "a" (Position 1 27),
        Token NewlineToken "\\n" (Position 1 28),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefDefaultsStmt
                "add"
                ["a", "b"]
                [("b", IntegerExpr 2 (Position 1 16))]
                [PrintStmt (IdentifierExpr "a" (Position 1 27)) (Position 1 21)]
                (Position 1 1)
            ]
        )

  it "parses keyword argument call" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IdentifierToken "a" (Position 1 9),
        Token AssignToken "=" (Position 1 10),
        Token IntegerToken "1" (Position 1 11),
        Token RParenToken ")" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "f" [KeywordArgExpr "a" (IntegerExpr 1 (Position 1 11)) (Position 1 9)] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses mixed positional then keyword argument call" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IntegerToken "1" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 13),
        Token IntegerToken "2" (Position 1 14),
        Token RParenToken ")" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "f"
                    [IntegerExpr 1 (Position 1 9), KeywordArgExpr "b" (IntegerExpr 2 (Position 1 14)) (Position 1 12)]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses positional then multiple keyword arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "f" (Position 1 7),
        Token LParenToken "(" (Position 1 8),
        Token IntegerToken "1" (Position 1 9),
        Token CommaToken "," (Position 1 10),
        Token IdentifierToken "b" (Position 1 12),
        Token AssignToken "=" (Position 1 13),
        Token IntegerToken "2" (Position 1 14),
        Token CommaToken "," (Position 1 15),
        Token IdentifierToken "c" (Position 1 17),
        Token AssignToken "=" (Position 1 18),
        Token IntegerToken "3" (Position 1 19),
        Token RParenToken ")" (Position 1 20),
        Token NewlineToken "\\n" (Position 1 21),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "f"
                    [ IntegerExpr 1 (Position 1 9),
                      KeywordArgExpr "b" (IntegerExpr 2 (Position 1 14)) (Position 1 12),
                      KeywordArgExpr "c" (IntegerExpr 3 (Position 1 19)) (Position 1 17)
                    ]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses multiple keyword arguments with trailing comma" $ do
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
        Token RParenToken ")" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "f"
                    [ KeywordArgExpr "a" (IntegerExpr 1 (Position 1 11)) (Position 1 9),
                      KeywordArgExpr "b" (IntegerExpr 2 (Position 1 16)) (Position 1 14)
                    ]
                    (Position 1 7)
                )
                (Position 1 1)
            ]
        )

  it "parses method-call syntax as function-style call" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "append" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IntegerToken "3" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token NewlineToken "\\n" (Position 1 18),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "append" [IdentifierExpr "x" (Position 1 7), IntegerExpr 3 (Position 1 16)] (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses chained method-call syntax" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "x" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "append" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IntegerToken "3" (Position 1 16),
        Token RParenToken ")" (Position 1 17),
        Token DotToken "." (Position 1 18),
        Token IdentifierToken "append" (Position 1 19),
        Token LParenToken "(" (Position 1 25),
        Token IntegerToken "4" (Position 1 26),
        Token RParenToken ")" (Position 1 27),
        Token NewlineToken "\\n" (Position 1 28),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                ( CallExpr
                    "append"
                    [ CallExpr "append" [IdentifierExpr "x" (Position 1 7), IntegerExpr 3 (Position 1 16)] (Position 1 9),
                      IntegerExpr 4 (Position 1 26)
                    ]
                    (Position 1 19)
                )
                (Position 1 1)
            ]
        )

  it "parses method-call syntax with multiple arguments" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "d" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "update" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IntegerToken "1" (Position 1 16),
        Token CommaToken "," (Position 1 17),
        Token IntegerToken "9" (Position 1 19),
        Token RParenToken ")" (Position 1 20),
        Token NewlineToken "\\n" (Position 1 21),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "update" [IdentifierExpr "d" (Position 1 7), IntegerExpr 1 (Position 1 16), IntegerExpr 9 (Position 1 19)] (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses method-call syntax with keyword argument" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "d" (Position 1 7),
        Token DotToken "." (Position 1 8),
        Token IdentifierToken "update" (Position 1 9),
        Token LParenToken "(" (Position 1 15),
        Token IdentifierToken "k" (Position 1 16),
        Token AssignToken "=" (Position 1 17),
        Token IntegerToken "1" (Position 1 18),
        Token RParenToken ")" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (CallExpr "update" [IdentifierExpr "d" (Position 1 7), KeywordArgExpr "k" (IntegerExpr 1 (Position 1 18)) (Position 1 16)] (Position 1 9))
                (Position 1 1)
            ]
        )

  it "parses return inside function body" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "id" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token ReturnToken "return" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [FunctionDefStmt "id" ["x"] [ReturnStmt (IdentifierExpr "x" (Position 1 1)) (Position 1 1)] (Position 1 1)])

  it "parses bare return inside function body as None" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "id" (Position 1 1),
        Token LParenToken "(" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token RParenToken ")" (Position 1 1),
        Token ColonToken ":" (Position 1 1),
        Token ReturnToken "return" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [FunctionDefStmt "id" ["x"] [ReturnStmt (NoneExpr (Position 1 1)) (Position 1 1)] (Position 1 1)])

  it "parses an indented function suite with multiple statements" $ do
    parseProgram
      [ Token DefToken "def" (Position 1 1),
        Token IdentifierToken "f" (Position 1 5),
        Token LParenToken "(" (Position 1 6),
        Token RParenToken ")" (Position 1 7),
        Token ColonToken ":" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token ReturnToken "return" (Position 3 3),
        Token IntegerToken "1" (Position 3 10),
        Token NewlineToken "\\n" (Position 3 11),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ FunctionDefStmt
                "f"
                []
                [ PassStmt (Position 2 3),
                  ReturnStmt (IntegerExpr 1 (Position 3 10)) (Position 3 3)
                ]
                (Position 1 1)
            ]
        )

  it "parses comparison with precedence (1 + 2 == 3)" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "1" (Position 1 1),
        Token PlusToken "+" (Position 1 1),
        Token IntegerToken "2" (Position 1 1),
        Token EqToken "==" (Position 1 1),
        Token IntegerToken "3" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [PrintStmt (BinaryExpr EqOperator (BinaryExpr AddOperator (IntegerExpr 1 (Position 1 1)) (IntegerExpr 2 (Position 1 1)) (Position 1 1)) (IntegerExpr 3 (Position 1 1)) (Position 1 1)) (Position 1 1)])

  it "parses not / and / or precedence" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token NotToken "not" (Position 1 1),
        Token IdentifierToken "x" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [PrintStmt (NotExpr (IdentifierExpr "x" (Position 1 1)) (Position 1 1)) (Position 1 1)])

  it "parses and/or as lower-precedence than comparison" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IdentifierToken "a" (Position 1 1),
        Token AndToken "and" (Position 1 1),
        Token IdentifierToken "b" (Position 1 1),
        Token OrToken "or" (Position 1 1),
        Token IdentifierToken "c" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 1),
        Token EOFToken "" (Position 1 1)
      ]
      `shouldBe` Right (Program [PrintStmt (BinaryExpr OrOperator (BinaryExpr AndOperator (IdentifierExpr "a" (Position 1 1)) (IdentifierExpr "b" (Position 1 1)) (Position 1 1)) (IdentifierExpr "c" (Position 1 1)) (Position 1 1)) (Position 1 1)])

  it "parses string literal expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token StringToken "hello" (Position 1 7),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (StringExpr "hello" (Position 1 7)) (Position 1 1)])

  it "parses True/False/None literals" $ do
    parseProgram
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
      `shouldBe` Right
        ( Program
            [ PrintStmt (IntegerExpr 1 (Position 1 7)) (Position 1 1),
              PrintStmt (IntegerExpr 0 (Position 2 7)) (Position 2 1),
              PrintStmt (NoneExpr (Position 3 7)) (Position 3 1)
            ]
        )

  it "parses for statement" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "i" (Position 1 5),
        Token InToken "in" (Position 1 7),
        Token IntegerToken "3" (Position 1 10),
        Token ColonToken ":" (Position 1 11),
        Token PrintToken "print" (Position 1 13),
        Token IdentifierToken "i" (Position 1 19),
        Token NewlineToken "\\n" (Position 1 20),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [ForStmt "i" (IntegerExpr 3 (Position 1 10)) [PrintStmt (IdentifierExpr "i" (Position 1 19)) (Position 1 13)] (Position 1 1)])

  it "parses an indented for suite with multiple statements" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "item" (Position 1 5),
        Token InToken "in" (Position 1 10),
        Token IdentifierToken "items" (Position 1 13),
        Token ColonToken ":" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token PassToken "pass" (Position 2 3),
        Token NewlineToken "\\n" (Position 2 7),
        Token PrintToken "print" (Position 3 3),
        Token IdentifierToken "item" (Position 3 9),
        Token NewlineToken "\\n" (Position 3 13),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token EOFToken "" (Position 4 1)
      ]
      `shouldBe` Right
        ( Program
            [ ForStmt
                "item"
                (IdentifierExpr "items" (Position 1 13))
                [ PassStmt (Position 2 3),
                  PrintStmt (IdentifierExpr "item" (Position 3 9)) (Position 3 3)
                ]
                (Position 1 1)
            ]
        )

  it "parses nested conditional inside for suite and continues" $ do
    parseProgram
      [ Token ForToken "for" (Position 1 1),
        Token IdentifierToken "item" (Position 1 5),
        Token InToken "in" (Position 1 10),
        Token IdentifierToken "items" (Position 1 13),
        Token ColonToken ":" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token IndentToken "<INDENT>" (Position 2 1),
        Token IfToken "if" (Position 2 3),
        Token IdentifierToken "inner" (Position 2 6),
        Token ColonToken ":" (Position 2 11),
        Token NewlineToken "\\n" (Position 2 12),
        Token IndentToken "<INDENT>" (Position 3 1),
        Token PrintToken "print" (Position 3 5),
        Token IntegerToken "1" (Position 3 11),
        Token NewlineToken "\\n" (Position 3 12),
        Token DedentToken "<DEDENT>" (Position 4 1),
        Token ElseToken "else" (Position 4 3),
        Token ColonToken ":" (Position 4 7),
        Token NewlineToken "\\n" (Position 4 8),
        Token IndentToken "<INDENT>" (Position 5 1),
        Token PrintToken "print" (Position 5 5),
        Token IntegerToken "2" (Position 5 11),
        Token NewlineToken "\\n" (Position 5 12),
        Token DedentToken "<DEDENT>" (Position 6 1),
        Token PassToken "pass" (Position 6 3),
        Token NewlineToken "\\n" (Position 6 7),
        Token DedentToken "<DEDENT>" (Position 7 1),
        Token PrintToken "print" (Position 7 1),
        Token IntegerToken "9" (Position 7 7),
        Token NewlineToken "\\n" (Position 7 8),
        Token EOFToken "" (Position 8 1)
      ]
      `shouldBe` Right
        ( Program
            [ ForStmt
                "item"
                (IdentifierExpr "items" (Position 1 13))
                [ IfStmt
                    (IdentifierExpr "inner" (Position 2 6))
                    [PrintStmt (IntegerExpr 1 (Position 3 11)) (Position 3 5)]
                    (Just [PrintStmt (IntegerExpr 2 (Position 5 11)) (Position 5 5)])
                    (Position 2 3),
                  PassStmt (Position 6 3)
                ]
                (Position 1 1),
              PrintStmt (IntegerExpr 9 (Position 7 7)) (Position 7 1)
            ]
        )

  it "parses break and continue statements" $ do
    parseProgram
      [ Token BreakToken "break" (Position 1 1),
        Token NewlineToken "\\n" (Position 1 6),
        Token ContinueToken "continue" (Position 2 1),
        Token NewlineToken "\\n" (Position 2 9),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldBe` Right (Program [BreakStmt (Position 1 1), ContinueStmt (Position 2 1)])

  it "parses list literal expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token CommaToken "," (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token RBracketToken "]" (Position 1 12),
        Token NewlineToken "\\n" (Position 1 13),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (ListExpr [IntegerExpr 1 (Position 1 8), IntegerExpr 2 (Position 1 11)] (Position 1 7)) (Position 1 1)])

  it "parses list literal expression with trailing comma" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBracketToken "[" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token CommaToken "," (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token RBracketToken "]" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (ListExpr [IntegerExpr 1 (Position 1 8), IntegerExpr 2 (Position 1 11)] (Position 1 7)) (Position 1 1)])

  it "parses dictionary literal expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBraceToken "{" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token ColonToken ":" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token IntegerToken "3" (Position 1 14),
        Token ColonToken ":" (Position 1 15),
        Token IntegerToken "4" (Position 1 17),
        Token RBraceToken "}" (Position 1 18),
        Token NewlineToken "\\n" (Position 1 19),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (DictExpr [(IntegerExpr 1 (Position 1 8), IntegerExpr 2 (Position 1 11)), (IntegerExpr 3 (Position 1 14), IntegerExpr 4 (Position 1 17))] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses dictionary literal expression with trailing comma" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token LBraceToken "{" (Position 1 7),
        Token IntegerToken "1" (Position 1 8),
        Token ColonToken ":" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token CommaToken "," (Position 1 12),
        Token RBraceToken "}" (Position 1 13),
        Token NewlineToken "\\n" (Position 1 14),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (DictExpr [(IntegerExpr 1 (Position 1 8), IntegerExpr 2 (Position 1 11))] (Position 1 7))
                (Position 1 1)
            ]
        )

  it "parses unary minus integer literal" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token MinusToken "-" (Position 1 7),
        Token IntegerToken "2" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right (Program [PrintStmt (IntegerExpr (-2) (Position 1 7)) (Position 1 1)])

  it "parses unary minus identifier and parenthesized expression" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token MinusToken "-" (Position 1 7),
        Token IdentifierToken "x" (Position 1 8),
        Token NewlineToken "\\n" (Position 1 9),
        Token PrintToken "print" (Position 2 1),
        Token MinusToken "-" (Position 2 7),
        Token LParenToken "(" (Position 2 8),
        Token IntegerToken "1" (Position 2 9),
        Token PlusToken "+" (Position 2 10),
        Token IntegerToken "2" (Position 2 11),
        Token RParenToken ")" (Position 2 12),
        Token NewlineToken "\\n" (Position 2 13),
        Token EOFToken "" (Position 3 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt (UnaryMinusExpr (IdentifierExpr "x" (Position 1 8)) (Position 1 7)) (Position 1 1),
              PrintStmt (UnaryMinusExpr (BinaryExpr AddOperator (IntegerExpr 1 (Position 2 9)) (IntegerExpr 2 (Position 2 11)) (Position 2 10)) (Position 2 7)) (Position 2 1)
            ]
        )

  it "parses multiplicative operators with precedence" $ do
    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "1" (Position 1 7),
        Token PlusToken "+" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token StarToken "*" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr AddOperator (IntegerExpr 1 (Position 1 7)) (BinaryExpr MultiplyOperator (IntegerExpr 2 (Position 1 11)) (IntegerExpr 3 (Position 1 15)) (Position 1 13)) (Position 1 9))
                (Position 1 1)
            ]
        )

    parseProgram
      [ Token PrintToken "print" (Position 1 1),
        Token IntegerToken "8" (Position 1 7),
        Token SlashToken "/" (Position 1 9),
        Token IntegerToken "2" (Position 1 11),
        Token PercentToken "%" (Position 1 13),
        Token IntegerToken "3" (Position 1 15),
        Token NewlineToken "\\n" (Position 1 16),
        Token EOFToken "" (Position 2 1)
      ]
      `shouldBe` Right
        ( Program
            [ PrintStmt
                (BinaryExpr ModuloOperator (BinaryExpr DivideOperator (IntegerExpr 8 (Position 1 7)) (IntegerExpr 2 (Position 1 11)) (Position 1 9)) (IntegerExpr 3 (Position 1 15)) (Position 1 13))
                (Position 1 1)
            ]
        )

