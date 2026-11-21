{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.FunctionEvalPart4Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, EqOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, PrintStmt, ReturnStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "function runtime - part 4" $ do
  it "does not evaluate default side effect when keyword argument is explicitly provided at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 54 9)) (Position 54 3),
                ReturnStmt (IdentifierExpr "x" (Position 55 10)) (Position 55 3)
              ]
              (Position 53 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IntegerExpr 2 (Position 56 21)] (Position 56 15)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 57 10)) (IdentifierExpr "b" (Position 57 14)) (Position 57 12)) (Position 57 3)]
              (Position 56 1),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 58 11), KeywordArgExpr "b" (IntegerExpr 3 (Position 58 16)) (Position 58 14)] (Position 58 7)) (Position 58 1)
          ]
      )
      `shouldBe` Right ["4"]

  it "evaluates default side effect when argument is omitted at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 60 9)) (Position 60 3),
                ReturnStmt (IdentifierExpr "x" (Position 61 10)) (Position 61 3)
              ]
              (Position 59 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IntegerExpr 2 (Position 62 21)] (Position 62 15)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 63 10)) (IdentifierExpr "b" (Position 63 14)) (Position 63 12)) (Position 63 3)]
              (Position 62 1),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 64 11)] (Position 64 7)) (Position 64 1)
          ]
      )
      `shouldBe` Right ["2", "3"]

  it "evaluates multiple omitted default expressions in parameter order at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 66 9)) (Position 66 3),
                ReturnStmt (IdentifierExpr "x" (Position 67 10)) (Position 67 3)
              ]
              (Position 65 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IntegerExpr 2 (Position 68 21)] (Position 68 15)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "probe" [IntegerExpr 3 (Position 68 33)] (Position 68 27)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 69 10)) (IdentifierExpr "b" (Position 69 14)) (Position 69 12))
                      (IdentifierExpr "c" (Position 69 18))
                      (Position 69 16)
                  )
                  (Position 69 3)
              ]
              (Position 68 1),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 70 11)] (Position 70 7)) (Position 70 1)
          ]
      )
      `shouldBe` Right ["2", "3", "6"]

  it "evaluates omitted defaults in parameter order when explicit and omitted arguments are interleaved at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 72 9)) (Position 72 3),
                ReturnStmt (IdentifierExpr "x" (Position 73 10)) (Position 73 3)
              ]
              (Position 71 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c", "d"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IntegerExpr 2 (Position 74 21)] (Position 74 15)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "probe" [IntegerExpr 30 (Position 74 34)] (Position 74 28)},
                FunctionDefault {functionDefaultParam = "d", functionDefaultExpr = CallExpr "probe" [IntegerExpr 4 (Position 74 47)] (Position 74 41)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      ( BinaryExpr
                          AddOperator
                          (BinaryExpr AddOperator (IdentifierExpr "a" (Position 75 10)) (IdentifierExpr "b" (Position 75 14)) (Position 75 12))
                          (IdentifierExpr "c" (Position 75 18))
                          (Position 75 16)
                      )
                      (IdentifierExpr "d" (Position 75 22))
                      (Position 75 20)
                  )
                  (Position 75 3)
              ]
              (Position 74 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ KeywordArgExpr "a" (CallExpr "probe" [IntegerExpr 1 (Position 76 19)] (Position 76 13)) (Position 76 11),
                    KeywordArgExpr "c" (CallExpr "probe" [IntegerExpr 3 (Position 76 31)] (Position 76 25)) (Position 76 23)
                  ]
                  (Position 76 7)
              )
              (Position 76 1)
          ]
      )
      `shouldBe` Right ["1", "3", "2", "4", "10"]
