{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart5Spec (spec) where

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
spec = describe "runtime error reporting - part 5" $ do
  it "reports Name error when default expression references later parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IdentifierExpr "c" (Position 34 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = IntegerExpr 2 (Position 34 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 35 10)) (IdentifierExpr "b" (Position 35 14)) (Position 35 12))
                      (IdentifierExpr "c" (Position 35 18))
                      (Position 35 16)
                  )
                  (Position 35 3)
              ]
              (Position 34 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 36 9)] (Position 36 7)) (Position 36 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier c at 34:14"

  it "reports Name error position from composite default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 38 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = BinaryExpr AddOperator (IdentifierExpr "b" (Position 38 21)) (IdentifierExpr "d" (Position 38 25)) (Position 38 23)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 39 10)) (IdentifierExpr "b" (Position 39 14)) (Position 39 12))
                      (IdentifierExpr "c" (Position 39 18))
                      (Position 39 16)
                  )
                  (Position 39 3)
              ]
              (Position 38 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 40 9)] (Position 40 7)) (Position 40 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier d at 38:25"

  it "reports builtin type error from default expression using bound parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "len" [IdentifierExpr "a" (Position 42 18)] (Position 42 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 43 10)) (IdentifierExpr "b" (Position 43 14)) (Position 43 12)) (Position 43 3)]
              (Position 42 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 44 9)] (Position 44 7)) (Position 44 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 42:14"

  it "prioritizes explicit argument evaluation error over default expression error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = BinaryExpr DivideOperator (IntegerExpr 1 (Position 46 14)) (IntegerExpr 0 (Position 46 18)) (Position 46 16)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 47 10)) (IdentifierExpr "b" (Position 47 14)) (Position 47 12)) (Position 47 3)]
              (Position 46 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 48 17)] (Position 48 13)) (Position 48 11)] (Position 48 7))
              (Position 48 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 48:13"

  it "prioritizes positional argument evaluation error over default expression error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = BinaryExpr DivideOperator (IntegerExpr 1 (Position 50 14)) (IntegerExpr 0 (Position 50 18)) (Position 50 16)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 51 10)) (IdentifierExpr "b" (Position 51 14)) (Position 51 12)) (Position 51 3)]
              (Position 50 1),
            PrintStmt
              (CallExpr "f" [CallExpr "len" [IntegerExpr 1 (Position 52 15)] (Position 52 11)] (Position 52 7))
              (Position 52 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 52:11"

  it "prioritizes explicit argument evaluation error over default builtin keyword rejection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 54 22)] (Position 54 21)) (Position 54 20)] (Position 54 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 55 10)) (IdentifierExpr "b" (Position 55 14)) (Position 55 12)) (Position 55 3)]
              (Position 54 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 56 17)] (Position 56 13)) (Position 56 11)] (Position 56 7))
              (Position 56 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 56:13"

  it "prioritizes positional argument evaluation error over default builtin keyword rejection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 58 22)] (Position 58 21)) (Position 58 20)] (Position 58 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 59 10)) (IdentifierExpr "b" (Position 59 14)) (Position 59 12)) (Position 59 3)]
              (Position 58 1),
            PrintStmt
              (CallExpr "f" [CallExpr "len" [IntegerExpr 1 (Position 60 15)] (Position 60 11)] (Position 60 7))
              (Position 60 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 60:11"

  it "prioritizes count mismatch over default builtin keyword rejection with keyword call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 62 24)] (Position 62 23)) (Position 62 21)] (Position 62 16)}]
              [ReturnStmt (IdentifierExpr "a" (Position 63 10)) (Position 63 3)]
              (Position 62 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "a" (IntegerExpr 1 (Position 64 11)) (Position 64 9)] (Position 64 7))
              (Position 64 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling f at 64:7"

  it "prioritizes count mismatch over default builtin keyword rejection with positional call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 66 24)] (Position 66 23)) (Position 66 21)] (Position 66 16)}]
              [ReturnStmt (IdentifierExpr "a" (Position 67 10)) (Position 67 3)]
              (Position 66 1),
            PrintStmt
              (CallExpr "f" [IntegerExpr 1 (Position 68 9)] (Position 68 7))
              (Position 68 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling f at 68:7"
