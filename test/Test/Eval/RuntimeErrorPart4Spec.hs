{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart4Spec (spec) where

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
spec = describe "runtime error reporting - part 4" $ do
  it "reports type error for len on unsupported argument" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "len" [IntegerExpr 1 (Position 13 9)] (Position 13 7)) (Position 13 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 13:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "len" [NoneExpr (Position 13 9)] (Position 13 7)) (Position 13 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 13:7"

  it "reports argument count mismatch for len" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "len" [] (Position 14 7)) (Position 14 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling len at 14:7"

  it "reports builtin keyword argument as unsupported at evaluator layer" $ do
    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "len"
                  [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 15 13)] (Position 15 12)) (Position 15 11)]
                  (Position 15 7)
              )
              (Position 15 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 15:11"

    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "len"
                  [KeywordArgExpr "x" (CallExpr "len" [IntegerExpr 1 (Position 16 17)] (Position 16 13)) (Position 16 11)]
                  (Position 16 7)
              )
              (Position 16 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 16:11"

    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "len"
                  [KeywordArgExpr "x" (IdentifierExpr "missing" (Position 17 13)) (Position 17 11)]
                  (Position 17 7)
              )
              (Position 17 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 17:11"

  it "reports method-style builtin keyword argument as unsupported at evaluator layer" $ do
    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "update"
                  [ DictExpr [] (Position 16 9),
                    KeywordArgExpr "k" (IntegerExpr 1 (Position 16 22)) (Position 16 20)
                  ]
                  (Position 16 7)
              )
              (Position 16 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 16:20"

    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "update"
                  [ DictExpr [] (Position 17 9),
                    KeywordArgExpr "k" (CallExpr "len" [IntegerExpr 1 (Position 17 27)] (Position 17 23)) (Position 17 20)
                  ]
                  (Position 17 7)
              )
              (Position 17 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 17:20"

  it "reports runtime error position from default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = BinaryExpr DivideOperator (IntegerExpr 1 (Position 18 14)) (IntegerExpr 0 (Position 18 18)) (Position 18 16)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 19 10)) (IdentifierExpr "b" (Position 19 14)) (Position 19 12)) (Position 19 3)]
              (Position 18 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 20 9)] (Position 20 7)) (Position 20 1)
          ]
      )
      `shouldBe` Left "Value error: division by zero at 18:16"

  it "reports Name error position from default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IdentifierExpr "missing" (Position 22 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 23 10)) (IdentifierExpr "b" (Position 23 14)) (Position 23 12)) (Position 23 3)]
              (Position 22 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 24 9)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 22:14"

  it "reports builtin type error position from default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "len" [IntegerExpr 1 (Position 26 18)] (Position 26 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 27 10)) (IdentifierExpr "b" (Position 27 14)) (Position 27 12)) (Position 27 3)]
              (Position 26 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 28 9)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 26:14"

  it "reports Name error when default expression references unknown identifier at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 30 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = IdentifierExpr "d" (Position 30 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 31 10)) (IdentifierExpr "b" (Position 31 14)) (Position 31 12))
                      (IdentifierExpr "c" (Position 31 18))
                      (Position 31 16)
                  )
                  (Position 31 3)
              ]
              (Position 30 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 32 9)] (Position 32 7)) (Position 32 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier d at 30:21"
