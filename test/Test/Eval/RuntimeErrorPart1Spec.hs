{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart1Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, DivideOperator, LtOperator, ModuloOperator, MultiplyOperator))
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr, NoneExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, IfStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runtime error reporting - part 1" $ do
  it "reports position for undefined identifier in if condition" $ do
    evalProgram
      ( Program
          [ IfStmt
              (IdentifierExpr "missing" (Position 3 8))
              [PrintStmt (IntegerExpr 1 (Position 3 10)) (Position 3 10)]
              Nothing
              (Position 3 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 3:8"

  it "reports position for undefined identifier in while condition" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 5 1)) (Position 5 1),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 6 7)) (IdentifierExpr "limit" (Position 6 11)) (Position 6 9))
              [PrintStmt (IdentifierExpr "x" (Position 7 9)) (Position 7 9)]
              (Position 6 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier limit at 6:11"

  it "reports position for return outside function" $ do
    evalProgram
      ( Program
          [ ReturnStmt (IntegerExpr 99 (Position 10 10)) (Position 10 3)
          ]
      )
      `shouldBe` Left "Return outside function at 10:3"

  it "reports position for undefined identifier read inside function" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "readMissing" [] [ReturnStmt (IdentifierExpr "missing" (Position 11 15)) (Position 11 8)] (Position 11 1),
            PrintStmt (CallExpr "readMissing" [] (Position 12 1)) (Position 12 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 11:15"

  it "reports duplicate keyword argument error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 2 10)) (Position 2 3)] (Position 1 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "b" (IntegerExpr 1 (Position 3 13)) (Position 3 11),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 3 18)) (Position 3 16)
                  ]
                  (Position 3 7)
              )
              (Position 3 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument b at 3:16"

  it "reports unexpected keyword argument error with mixed positional call" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 5 10)) (Position 5 3)] (Position 4 1),
            PrintStmt
              (CallExpr "f" [IntegerExpr 1 (Position 6 9), KeywordArgExpr "b" (IntegerExpr 2 (Position 6 16)) (Position 6 14)] (Position 6 7))
              (Position 6 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument b at 6:14"

  it "reports first unexpected keyword in source order at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 7 10)) (Position 7 3)] (Position 7 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 8 9),
                    KeywordArgExpr "z" (IntegerExpr 2 (Position 8 14)) (Position 8 12),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 8 21)) (Position 8 19)
                  ]
                  (Position 8 7)
              )
              (Position 8 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument z at 8:12"

  it "reports unexpected keyword argument for keyword-only call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 9 10)) (Position 9 3)] (Position 9 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "z" (IntegerExpr 1 (Position 10 11)) (Position 10 9),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 10 16)) (Position 10 14)
                  ]
                  (Position 10 7)
              )
              (Position 10 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument z at 10:9"

  it "prioritizes keyword argument expression type error over unexpected keyword" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 9 10)) (Position 9 3)] (Position 9 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 10 17)] (Position 10 13)) (Position 10 11)] (Position 10 7))
              (Position 10 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 10:13"

  it "prioritizes keyword argument expression Name error over unexpected keyword" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 11 10)) (Position 11 3)] (Position 11 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "b" (IdentifierExpr "missing" (Position 12 11)) (Position 12 11)] (Position 12 7))
              (Position 12 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 12:11"

  it "reports multiple-values conflict at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 8 10)) (Position 8 3)] (Position 7 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 9 9),
                    IntegerExpr 2 (Position 9 12),
                    KeywordArgExpr "a" (IntegerExpr 3 (Position 9 20)) (Position 9 18)
                  ]
                  (Position 9 7)
              )
              (Position 9 1)
          ]
      )
      `shouldBe` Left "Argument error: multiple values for parameter a at 9:18"

  it "reports leftmost multiple-values conflict when multiple parameters collide" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (IdentifierExpr "a" (Position 10 10)) (Position 10 3)] (Position 10 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 11 9),
                    IntegerExpr 2 (Position 11 12),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 11 17)) (Position 11 15),
                    KeywordArgExpr "a" (IntegerExpr 4 (Position 11 24)) (Position 11 22)
                  ]
                  (Position 11 7)
              )
              (Position 11 1)
          ]
      )
      `shouldBe` Left "Argument error: multiple values for parameter b at 11:15"
