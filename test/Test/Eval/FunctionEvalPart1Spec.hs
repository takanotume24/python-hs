{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.FunctionEvalPart1Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, EqOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr))
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, PrintStmt, ReturnStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "function runtime - part 1" $ do
  it "supports recursion with accumulating argument" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "climb"
              ["n", "x"]
              [ IfStmt
                  (BinaryExpr EqOperator (IdentifierExpr "x" (Position 1 1)) (IdentifierExpr "n" (Position 1 1)) (Position 1 1))
                  [ReturnStmt (IdentifierExpr "x" (Position 1 1)) (Position 1 1)]
                  ( Just
                      [ ReturnStmt
                          ( CallExpr
                              "climb"
                              [ IdentifierExpr "n" (Position 1 1),
                                BinaryExpr AddOperator (IdentifierExpr "x" (Position 1 1)) (IntegerExpr 1 (Position 1 1)) (Position 1 1)
                              ]
                              (Position 1 1)
                          )
                          (Position 1 1)
                      ]
                  )
                  (Position 1 1)
              ]
              (Position 1 1),
            PrintStmt (CallExpr "climb" [IntegerExpr 3 (Position 1 1), IntegerExpr 0 (Position 1 1)] (Position 1 1)) (Position 1 1)
          ]
      )
      `shouldBe` Right ["3"]

  it "does not leak function-local assignments to outer scope" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 10 (Position 2 1)) (Position 2 1),
            FunctionDefStmt
              "setLocal"
              []
              [ AssignStmt "x" (IntegerExpr 99 (Position 3 3)) (Position 3 3),
                ReturnStmt (IdentifierExpr "x" (Position 3 10)) (Position 3 10)
              ]
              (Position 3 1),
            PrintStmt (CallExpr "setLocal" [] (Position 4 1)) (Position 4 1),
            PrintStmt (IdentifierExpr "x" (Position 5 1)) (Position 5 1)
          ]
      )
      `shouldBe` Right ["99", "10"]

  it "allows function body to read global variable" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 7 (Position 6 1)) (Position 6 1),
            FunctionDefStmt
              "readGlobal"
              []
              [ ReturnStmt (IdentifierExpr "x" (Position 7 10)) (Position 7 10)
              ]
              (Position 7 1),
            PrintStmt (CallExpr "readGlobal" [] (Position 8 1)) (Position 8 1)
          ]
      )
      `shouldBe` Right ["7"]

  it "prefers function parameter over global with same name" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 7 (Position 9 1)) (Position 9 1),
            FunctionDefStmt
              "echo"
              ["x"]
              [ ReturnStmt (IdentifierExpr "x" (Position 10 10)) (Position 10 10)
              ]
              (Position 10 1),
            PrintStmt (CallExpr "echo" [IntegerExpr 99 (Position 11 12)] (Position 11 1)) (Position 11 1)
          ]
      )
      `shouldBe` Right ["99"]

  it "updates global variable when declared with global inside function" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 10 (Position 12 1)) (Position 12 1),
            FunctionDefStmt
              "setGlobal"
              []
              [ GlobalStmt "x" (Position 13 3),
                AssignStmt "x" (IntegerExpr 99 (Position 14 3)) (Position 14 3),
                ReturnStmt (IdentifierExpr "x" (Position 15 10)) (Position 15 10)
              ]
              (Position 13 1),
            PrintStmt (CallExpr "setGlobal" [] (Position 16 1)) (Position 16 1),
            PrintStmt (IdentifierExpr "x" (Position 17 1)) (Position 17 1)
          ]
      )
      `shouldBe` Right ["99", "99"]

  it "creates a new global variable when declared in function" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "makeGlobal"
              []
              [ GlobalStmt "y" (Position 18 3),
                AssignStmt "y" (IntegerExpr 5 (Position 19 3)) (Position 19 3)
              ]
              (Position 18 1),
            PrintStmt (CallExpr "makeGlobal" [] (Position 20 1)) (Position 20 1),
            PrintStmt (IdentifierExpr "y" (Position 21 1)) (Position 21 1)
          ]
      )
      `shouldBe` Right ["0", "5"]

  it "treats global declaration in conditional branch as function-wide" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 1 (Position 22 1)) (Position 22 1),
            FunctionDefStmt
              "setViaBranch"
              []
              [ IfStmt
                  (IntegerExpr 0 (Position 23 6))
                  [GlobalStmt "x" (Position 24 5)]
                  Nothing
                  (Position 23 3),
                AssignStmt "x" (IntegerExpr 2 (Position 25 3)) (Position 25 3)
              ]
              (Position 23 1),
            PrintStmt (CallExpr "setViaBranch" [] (Position 26 1)) (Position 26 1),
            PrintStmt (IdentifierExpr "x" (Position 27 1)) (Position 27 1)
          ]
      )
      `shouldBe` Right ["0", "2"]

  it "evaluates keyword call arguments left-to-right at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt
              "probe"
              ["x"]
              [ PrintStmt (IdentifierExpr "x" (Position 30 9)) (Position 30 3),
                ReturnStmt (IdentifierExpr "x" (Position 31 10)) (Position 31 3)
              ]
              (Position 29 1),
            FunctionDefStmt
              "add"
              ["a", "b"]
              [ ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 33 10)) (IdentifierExpr "b" (Position 33 14)) (Position 33 12)) (Position 33 3)
              ]
              (Position 32 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ KeywordArgExpr "a" (CallExpr "probe" [IntegerExpr 1 (Position 34 19)] (Position 34 13)) (Position 34 11),
                    KeywordArgExpr "b" (CallExpr "probe" [IntegerExpr 2 (Position 34 31)] (Position 34 25)) (Position 34 23)
                  ]
                  (Position 34 7)
              )
              (Position 34 1)
          ]
      )
      `shouldBe` Right ["1", "2", "3"]
