module Test.Eval.FunctionEvalSpec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, EqOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, PrintStmt, ReturnStmt))
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
              [ ("b", CallExpr "probe" [IntegerExpr 2 (Position 44 21)] (Position 44 15)),
                ("c", CallExpr "probe" [IntegerExpr 3 (Position 44 33)] (Position 44 27))
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

  it "evaluates default expression using global value at call time at evaluator layer" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 10 (Position 48 1)) (Position 48 1),
            FunctionDefDefaultsStmt
              "add"
              ["a", "b"]
              [("b", IdentifierExpr "x" (Position 49 14))]
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
              [ ("b", IntegerExpr 2 (Position 53 14)),
                ("c", IdentifierExpr "b" (Position 53 21))
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

  it "evaluates default expression referencing explicit argument parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "add"
              ["a", "b", "c"]
              [ ("b", IntegerExpr 2 (Position 57 14)),
                ("c", IdentifierExpr "a" (Position 57 21))
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
              [("b", CallExpr "probe" [IntegerExpr 2 (Position 56 21)] (Position 56 15))]
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
              [("b", CallExpr "probe" [IntegerExpr 2 (Position 62 21)] (Position 62 15))]
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
              [ ("b", CallExpr "probe" [IntegerExpr 2 (Position 68 21)] (Position 68 15)),
                ("c", CallExpr "probe" [IntegerExpr 3 (Position 68 33)] (Position 68 27))
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
              [ ("b", CallExpr "probe" [IntegerExpr 2 (Position 74 21)] (Position 74 15)),
                ("c", CallExpr "probe" [IntegerExpr 30 (Position 74 34)] (Position 74 28)),
                ("d", CallExpr "probe" [IntegerExpr 4 (Position 74 47)] (Position 74 41))
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
