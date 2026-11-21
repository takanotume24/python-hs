{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.FunctionEvalPart2Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, EqOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, PrintStmt, ReturnStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "function runtime - part 2" $ do
  it "evaluates mixed positional and keyword call arguments left-to-right at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 36 9)) (Position 36 3),
                ReturnStmt (IdentifierExpr "x" (Position 37 10)) (Position 37 3)
              ]
              (Position 35 1),
            FunctionDefStmt
              "add"
              ["a", "b"]
              [ ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 39 10)) (IdentifierExpr "b" (Position 39 14)) (Position 39 12)) (Position 39 3)
              ]
              (Position 38 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ CallExpr "probe" [IntegerExpr 1 (Position 40 17)] (Position 40 11),
                    KeywordArgExpr "b" (CallExpr "probe" [IntegerExpr 2 (Position 40 29)] (Position 40 23)) (Position 40 21)
                  ]
                  (Position 40 7)
              )
              (Position 40 1)
          ]
      )
      `shouldBe` Right ["1", "2", "3"]

  it "evaluates explicit keyword arguments before missing defaults at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 42 9)) (Position 42 3),
                ReturnStmt (IdentifierExpr "x" (Position 43 10)) (Position 43 3)
              ]
              (Position 41 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IntegerExpr 2 (Position 44 21)] (Position 44 15)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "probe" [IntegerExpr 3 (Position 44 33)] (Position 44 27)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 45 10)) (IdentifierExpr "b" (Position 45 14)) (Position 45 12))
                      (IdentifierExpr "c" (Position 45 18))
                      (Position 45 16)
                  )
                  (Position 45 3)
              ]
              (Position 44 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ KeywordArgExpr "a" (CallExpr "probe" [IntegerExpr 1 (Position 46 19)] (Position 46 13)) (Position 46 11),
                    KeywordArgExpr "c" (CallExpr "probe" [IntegerExpr 4 (Position 46 31)] (Position 46 25)) (Position 46 23)
                  ]
                  (Position 46 7)
              )
              (Position 46 1)
          ]
      )
      `shouldBe` Right ["1", "4", "2", "7"]

  it "evaluates keyword argument before default expression that references it at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 47 9)) (Position 47 3),
                ReturnStmt (IdentifierExpr "x" (Position 48 10)) (Position 48 3)
              ]
              (Position 46 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IdentifierExpr "a" (Position 49 22)] (Position 49 16)}]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 50 10)) (IdentifierExpr "b" (Position 50 14)) (Position 50 12)) (Position 50 3)]
              (Position 49 1),
            PrintStmt
              (CallExpr "add" [KeywordArgExpr "a" (CallExpr "probe" [IntegerExpr 1 (Position 51 25)] (Position 51 19)) (Position 51 17)] (Position 51 7))
              (Position 51 1)
          ]
      )
      `shouldBe` Right ["1", "1", "2"]

  it "evaluates positional and keyword arguments before omitted default expressions in mixed call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 53 9)) (Position 53 3),
                ReturnStmt (IdentifierExpr "x" (Position 54 10)) (Position 54 3)
              ]
              (Position 52 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = CallExpr "probe" [IdentifierExpr "a" (Position 55 22)] (Position 55 16)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "probe" [IntegerExpr 30 (Position 55 34)] (Position 55 28)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 56 10)) (IdentifierExpr "b" (Position 56 14)) (Position 56 12))
                      (IdentifierExpr "c" (Position 56 18))
                      (Position 56 16)
                  )
                  (Position 56 3)
              ]
              (Position 55 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ CallExpr "probe" [IntegerExpr 1 (Position 57 17)] (Position 57 11),
                    KeywordArgExpr "c" (CallExpr "probe" [IntegerExpr 3 (Position 57 29)] (Position 57 23)) (Position 57 21)
                  ]
                  (Position 57 7)
              )
              (Position 57 1)
          ]
      )
      `shouldBe` Right ["1", "3", "1", "5"]

  it "evaluates default expression using global value at call time at evaluator layer" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 10 (Position 48 1)) (Position 48 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b"]
              [FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IdentifierExpr "x" (Position 49 14)}]
              [ ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 50 10)) (IdentifierExpr "b" (Position 50 14)) (Position 50 12)) (Position 50 3)
              ]
              (Position 49 1),
            AssignStmt "x" (IntegerExpr 20 (Position 51 1)) (Position 51 1),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 52 11)] (Position 52 7)) (Position 52 1)
          ]
      )
      `shouldBe` Right ["21"]

  it "evaluates default expression referencing earlier default parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ FunctionDefault {functionDefaultParam = "b", functionDefaultExpr = IntegerExpr 2 (Position 53 14)},
                FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = IdentifierExpr "b" (Position 53 21)}
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 54 10)) (IdentifierExpr "b" (Position 54 14)) (Position 54 12))
                      (IdentifierExpr "c" (Position 54 18))
                      (Position 54 16)
                  )
                  (Position 54 3)
              ]
              (Position 53 1),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 55 11)] (Position 55 7)) (Position 55 1)
          ]
      )
      `shouldBe` Right ["5"]
