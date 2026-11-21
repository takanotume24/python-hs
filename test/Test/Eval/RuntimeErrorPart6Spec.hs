{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart6Spec (spec) where

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
spec = describe "runtime error reporting - part 6" $ do
  it "prioritizes leftmost argument evaluation error in mixed positional and keyword call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 54 10)) (IdentifierExpr "b" (Position 54 14)) (Position 54 12)) (Position 54 3)] (Position 53 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ CallExpr "len" [IntegerExpr 1 (Position 55 13)] (Position 55 9),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 55 24)] (Position 55 20)) (Position 55 18)
                  ]
                  (Position 55 7)
              )
              (Position 55 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 55:9"

  it "prioritizes leftmost argument evaluation error in keyword-only call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 57 10)) (IdentifierExpr "b" (Position 57 14)) (Position 57 12)) (Position 57 3)] (Position 56 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 58 17)] (Position 58 13)) (Position 58 11),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 58 27)] (Position 58 23)) (Position 58 21)
                  ]
                  (Position 58 7)
              )
              (Position 58 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 58:13"

  it "reports right keyword argument evaluation error after left keyword succeeds at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 60 10)) (IdentifierExpr "b" (Position 60 14)) (Position 60 12)) (Position 60 3)] (Position 59 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 61 13)) (Position 61 11),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 61 23)] (Position 61 19)) (Position 61 17)
                  ]
                  (Position 61 7)
              )
              (Position 61 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 61:19"

  it "reports keyword argument evaluation error after positional argument succeeds at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 63 10)) (IdentifierExpr "b" (Position 63 14)) (Position 63 12)) (Position 63 3)] (Position 62 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 64 9),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 64 19)] (Position 64 15)) (Position 64 13)
                  ]
                  (Position 64 7)
              )
              (Position 64 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 64:15"

  it "reports Name error position inside builtin default list expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "len" [ListExpr [IdentifierExpr "a" (Position 46 19), IdentifierExpr "missing" (Position 46 22)] (Position 46 18)] (Position 46 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 47 10)) (IdentifierExpr "b" (Position 47 14)) (Position 47 12)) (Position 47 3)]
              (Position 46 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 48 9)] (Position 48 7)) (Position 48 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 46:22"

  it "reports bool builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "bool" [] (Position 14 7)) (Position 14 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling bool at 14:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "bool" [IntegerExpr 1 (Position 15 12), IntegerExpr 2 (Position 15 15)] (Position 15 7)) (Position 15 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling bool at 15:7"

  it "reports break/continue outside loop" $ do
    evalProgram (Program [BreakStmt (Position 15 3)]) `shouldBe` Left "Break outside loop at 15:3"
    evalProgram (Program [ContinueStmt (Position 16 3)]) `shouldBe` Left "Continue outside loop at 16:3"

  it "reports range builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "range" [StringExpr "x" (Position 17 13)] (Position 17 7)) (Position 17 1)
          ]
      )
      `shouldBe` Left "Type error: range expects int at 17:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "range" [] (Position 18 7)) (Position 18 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling range at 18:7"

  it "reports append builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "append" [IntegerExpr 1 (Position 19 14), IntegerExpr 2 (Position 19 17)] (Position 19 7)) (Position 19 1)
          ]
      )
      `shouldBe` Left "Type error: append expects list as first argument at 19:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "append" [ListExpr [IntegerExpr 1 (Position 20 15)] (Position 20 14)] (Position 20 7)) (Position 20 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling append at 20:7"
