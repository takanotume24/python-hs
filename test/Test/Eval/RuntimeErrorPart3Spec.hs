{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart3Spec (spec) where

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
spec = describe "runtime error reporting - part 3" $ do
  it "prioritizes call-site binding errors over default builtin keyword rejection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 18 24)] (Position 18 23)) (Position 18 21)] (Position 18 16)}]
              [ReturnStmt (IdentifierExpr "a" (Position 19 10)) (Position 19 3)]
              (Position 18 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 20 13)) (Position 20 11),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 20 20)) (Position 20 18),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 20 27)) (Position 20 25)
                  ]
                  (Position 20 7)
              )
              (Position 20 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 20:18"

    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 22 24)] (Position 22 23)) (Position 22 21)] (Position 22 16)}]
              [ReturnStmt (IdentifierExpr "a" (Position 23 10)) (Position 23 3)]
              (Position 22 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 24 13)) (Position 24 11),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 24 20)) (Position 24 18),
                    KeywordArgExpr "d" (IntegerExpr 3 (Position 24 27)) (Position 24 25)
                  ]
                  (Position 24 7)
              )
              (Position 24 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument d at 24:25"

    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [FunctionDefault {functionDefaultParam = "c", functionDefaultExpr = CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 26 24)] (Position 26 23)) (Position 26 21)] (Position 26 16)}]
              [ReturnStmt (IdentifierExpr "a" (Position 27 10)) (Position 27 3)]
              (Position 26 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 28 9),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 28 14)) (Position 28 12),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 28 21)) (Position 28 19)
                  ]
                  (Position 28 7)
              )
              (Position 28 1)
          ]
      )
      `shouldBe` Left "Argument error: multiple values for parameter a at 28:12"

  it "prioritizes unexpected keyword error over multiple values error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 22 10)) (Position 22 3)] (Position 22 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 23 9),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 23 14)) (Position 23 12),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 23 21)) (Position 23 19)
                  ]
                  (Position 23 7)
              )
              (Position 23 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument b at 23:19"

  it "reports user-defined count mismatch at call-site position at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "add" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 32 10)) (Position 32 3)] (Position 31 1),
            PrintStmt
              (CallExpr "add" [IntegerExpr 1 (Position 33 11), IntegerExpr 2 (Position 33 14)] (Position 33 7))
              (Position 33 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling add at 33:7"

    evalProgram
      ( Program
          [ FunctionDefStmt "add" ["a", "b"] [ReturnStmt (IdentifierExpr "a" (Position 35 10)) (Position 35 3)] (Position 34 1),
            PrintStmt
              (CallExpr "add" [KeywordArgExpr "a" (IntegerExpr 1 (Position 36 13)) (Position 36 11)] (Position 36 7))
              (Position 36 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling add at 36:7"

    evalProgram
      ( Program
          [ FunctionDefStmt "add" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 38 10)) (Position 38 3)] (Position 37 1),
            PrintStmt
              ( CallExpr
                  "add"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 39 13)) (Position 39 11),
                    IntegerExpr 2 (Position 39 16)
                  ]
                  (Position 39 7)
              )
              (Position 39 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling add at 39:7"

  it "reports clear type error for unsupported mixed types in plus" $ do
    evalProgram
      ( Program
          [ PrintStmt (BinaryExpr AddOperator (StringExpr "hello" (Position 12 8)) (IntegerExpr 1 (Position 12 18)) (Position 12 14)) (Position 12 1)
          ]
      )
      `shouldBe` Left "Type error: + expects int+int or string+string at 12:14"

    evalProgram
      ( Program
          [ PrintStmt (BinaryExpr AddOperator (ListExpr [] (Position 12 8)) (IntegerExpr 1 (Position 12 18)) (Position 12 14)) (Position 12 1)
          ]
      )
      `shouldBe` Left "Type error: + expects int+int or string+string at 12:14"

    evalProgram
      ( Program
          [ PrintStmt (BinaryExpr AddOperator (NoneExpr (Position 12 8)) (IntegerExpr 1 (Position 12 18)) (Position 12 14)) (Position 12 1)
          ]
      )
      `shouldBe` Left "Type error: + expects int+int or string+string at 12:14"

  it "reports multiplicative operator errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (BinaryExpr MultiplyOperator (StringExpr "x" (Position 12 8)) (IntegerExpr 2 (Position 12 14)) (Position 12 12)) (Position 12 1)
          ]
      )
      `shouldBe` Left "Type error: expected int in * at 12:12"

    evalProgram
      ( Program
          [ PrintStmt (BinaryExpr DivideOperator (IntegerExpr 8 (Position 13 8)) (IntegerExpr 0 (Position 13 12)) (Position 13 10)) (Position 13 1)
          ]
      )
      `shouldBe` Left "Value error: division by zero at 13:10"

    evalProgram
      ( Program
          [ PrintStmt (BinaryExpr ModuloOperator (IntegerExpr 8 (Position 14 8)) (IntegerExpr 0 (Position 14 12)) (Position 14 10)) (Position 14 1)
          ]
      )
      `shouldBe` Left "Value error: modulo by zero at 14:10"
