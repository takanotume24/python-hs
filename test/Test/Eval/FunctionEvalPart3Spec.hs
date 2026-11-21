{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.FunctionEvalPart3Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, EqOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, PrintStmt, ReturnStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "function runtime - part 3" $ do
  it "evaluates default expression referencing explicit argument parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 57 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = IdentifierExpr "a" (Position 57 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 58 10)) (IdentifierExpr "b" (Position 58 14)) (Position 58 12))
                      (IdentifierExpr "c" (Position 58 18))
                      (Position 58 16)
                  )
                  (Position 58 3)
              ]
              (Position 57 1),
            PrintStmt (CallExpr "add" [IntegerExpr 3 (Position 59 11)] (Position 59 7)) (Position 59 1)
          ]
      )
      `shouldBe` Right ["8"]

  it "evaluates default expression referencing parameter bound by keyword argument at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "add"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IdentifierExpr "a" (Position 60 14)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 61 10)) (IdentifierExpr "b" (Position 61 14)) (Position 61 12)) (Position 61 3)]
              (Position 60 1),
            PrintStmt
              (CallExpr "add" [KeywordArgExpr "a" (IntegerExpr 3 (Position 62 13)) (Position 62 11)] (Position 62 7))
              (Position 62 1)
          ]
      )
      `shouldBe` Right ["6"]

  it "evaluates composite default expression referencing bound values at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 61 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = BinaryExpr AddOperator (IdentifierExpr "b" (Position 61 21)) (IdentifierExpr "a" (Position 61 25)) (Position 61 23)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 62 10)) (IdentifierExpr "b" (Position 62 14)) (Position 62 12))
                      (IdentifierExpr "c" (Position 62 18))
                      (Position 62 16)
                  )
                  (Position 62 3)
              ]
              (Position 61 1),
            PrintStmt (CallExpr "add" [IntegerExpr 3 (Position 63 11)] (Position 63 7)) (Position 63 1)
          ]
      )
      `shouldBe` Right ["10"]

  it "evaluates builtin call in default expression using bound values at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 65 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "len" [ListExpr [IdentifierExpr "a" (Position 65 26), IdentifierExpr "b" (Position 65 29)] (Position 65 25)] (Position 65 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 66 10)) (IdentifierExpr "b" (Position 66 14)) (Position 66 12))
                      (IdentifierExpr "c" (Position 66 18))
                      (Position 66 16)
                  )
                  (Position 66 3)
              ]
              (Position 65 1),
            PrintStmt (CallExpr "add" [IntegerExpr 3 (Position 67 11)] (Position 67 7)) (Position 67 1)
          ]
      )
      `shouldBe` Right ["7"]

  it "evaluates builtin default expression with side effect when omitted at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 69 9)) (Position 69 3),
                ReturnStmt (IdentifierExpr "x" (Position 70 10)) (Position 70 3)
              ]
              (Position 68 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 71 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "probe" [CallExpr "len" [ListExpr [IdentifierExpr "a" (Position 71 32), IdentifierExpr "b" (Position 71 35)] (Position 71 31)] (Position 71 27)] (Position 71 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 72 10)) (IdentifierExpr "b" (Position 72 14)) (Position 72 12))
                      (IdentifierExpr "c" (Position 72 18))
                      (Position 72 16)
                  )
                  (Position 72 3)
              ]
              (Position 71 1),
            PrintStmt (CallExpr "add" [IntegerExpr 3 (Position 73 11)] (Position 73 7)) (Position 73 1)
          ]
      )
      `shouldBe` Right ["2", "7"]

  it "does not evaluate builtin default expression with side effect when explicitly overridden at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 75 9)) (Position 75 3),
                ReturnStmt (IdentifierExpr "x" (Position 76 10)) (Position 76 3)
              ]
              (Position 74 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 77 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "probe" [CallExpr "len" [ListExpr [IdentifierExpr "a" (Position 77 32), IdentifierExpr "b" (Position 77 35)] (Position 77 31)] (Position 77 27)] (Position 77 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 78 10)) (IdentifierExpr "b" (Position 78 14)) (Position 78 12))
                      (IdentifierExpr "c" (Position 78 18))
                      (Position 78 16)
                  )
                  (Position 78 3)
              ]
              (Position 77 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ IntegerExpr 3 (Position 79 11),
                    KeywordArgExpr "c" (CallExpr "probe" [IntegerExpr 9 (Position 79 23)] (Position 79 17)) (Position 79 15)
                  ]
                  (Position 79 7)
              )
              (Position 79 1)
          ]
      )
      `shouldBe` Right ["9", "14"]
