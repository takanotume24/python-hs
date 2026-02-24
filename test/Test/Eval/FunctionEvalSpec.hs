module Test.Eval.FunctionEvalSpec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, EqOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefStmt, IfStmt, PrintStmt, ReturnStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "function runtime" $ do
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
