{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart2Spec (spec) where

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
spec = describe "runtime error reporting - part 2" $ do
  it "prioritizes keyword argument expression type error over multiple-values detection" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 9 10)) (Position 9 3)] (Position 9 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 11 9),
                    KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 11 20)] (Position 11 16)) (Position 11 14)
                  ]
                  (Position 11 7)
              )
              (Position 11 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 11:16"

  it "prioritizes keyword argument expression Name error over multiple-values detection" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 12 10)) (Position 12 3)] (Position 12 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 13 9),
                    KeywordArgExpr "a" (IdentifierExpr "missing" (Position 13 16)) (Position 13 14)
                  ]
                  (Position 13 7)
              )
              (Position 13 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 13:16"

  it "reports builtin keyword rejection precedence in mixed calls at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 14 10)) (Position 14 3)] (Position 14 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 15 9),
                    KeywordArgExpr "a" (CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 15 24)] (Position 15 23)) (Position 15 21)] (Position 15 16)) (Position 15 14)
                  ]
                  (Position 15 7)
              )
              (Position 15 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 15:21"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 16 10)) (Position 16 3)] (Position 16 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 17 9),
                    KeywordArgExpr "a" (CallExpr "len" [IdentifierExpr "missing" (Position 17 20)] (Position 17 16)) (Position 17 14)
                  ]
                  (Position 17 7)
              )
              (Position 17 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 17:20"

  it "prioritizes first keyword evaluation error over duplicate keyword detection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 12 10)) (Position 12 3)] (Position 11 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 13 17)] (Position 13 13)) (Position 13 11),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 13 22)) (Position 13 20)
                  ]
                  (Position 13 7)
              )
              (Position 13 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 13:13"

  it "prioritizes duplicate keyword detection over second keyword evaluation error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 15 10)) (Position 15 3)] (Position 14 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 16 13)) (Position 16 11),
                    KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 16 24)] (Position 16 20)) (Position 16 18)
                  ]
                  (Position 16 7)
              )
              (Position 16 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 16:18"

  it "prioritizes first keyword Name error over duplicate keyword detection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 17 10)) (Position 17 3)] (Position 17 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IdentifierExpr "missing" (Position 18 11)) (Position 18 11),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 18 22)) (Position 18 20)
                  ]
                  (Position 18 7)
              )
              (Position 18 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 18:11"

  it "prioritizes duplicate keyword detection over second keyword Name error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 20 10)) (Position 20 3)] (Position 19 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 21 13)) (Position 21 11),
                    KeywordArgExpr "a" (IdentifierExpr "missing" (Position 21 20)) (Position 21 18)
                  ]
                  (Position 21 7)
              )
              (Position 21 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 21:18"

  it "reports first duplicate keyword argument in source order at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (IdentifierExpr "a" (Position 30 10)) (Position 30 3)] (Position 29 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 31 13)) (Position 31 11),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 31 20)) (Position 31 18),
                    KeywordArgExpr "a" (IntegerExpr 3 (Position 31 27)) (Position 31 25),
                    KeywordArgExpr "b" (IntegerExpr 4 (Position 31 34)) (Position 31 32)
                  ]
                  (Position 31 7)
              )
              (Position 31 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 31:25"
