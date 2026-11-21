{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart9Spec (spec) where

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
spec = describe "runtime error reporting - part 9" $ do
  it "reports slash-assign errors" $ do
    evalProgram
      ( Program
          [ DivAssignStmt "x" (IntegerExpr 2 (Position 39 6)) (Position 39 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier x at 39:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (StringExpr "a" (Position 40 5)) (Position 40 1),
            DivAssignStmt "x" (IntegerExpr 2 (Position 41 6)) (Position 41 1)
          ]
      )
      `shouldBe` Left "Type error: expected int in /= at 41:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 8 (Position 42 5)) (Position 42 1),
            DivAssignStmt "x" (IntegerExpr 0 (Position 43 6)) (Position 43 1)
          ]
      )
      `shouldBe` Left "Value error: division by zero at 43:1"

  it "reports percent-assign errors" $ do
    evalProgram
      ( Program
          [ ModAssignStmt "x" (IntegerExpr 2 (Position 44 6)) (Position 44 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier x at 44:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (StringExpr "a" (Position 45 5)) (Position 45 1),
            ModAssignStmt "x" (IntegerExpr 2 (Position 46 6)) (Position 46 1)
          ]
      )
      `shouldBe` Left "Type error: expected int in %= at 46:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 8 (Position 47 5)) (Position 47 1),
            ModAssignStmt "x" (IntegerExpr 0 (Position 48 6)) (Position 48 1)
          ]
      )
      `shouldBe` Left "Value error: modulo by zero at 48:1"

  it "reports double-slash-assign errors" $ do
    evalProgram
      ( Program
          [ FloorDivAssignStmt "x" (IntegerExpr 2 (Position 49 7)) (Position 49 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier x at 49:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (StringExpr "a" (Position 50 5)) (Position 50 1),
            FloorDivAssignStmt "x" (IntegerExpr 2 (Position 51 7)) (Position 51 1)
          ]
      )
      `shouldBe` Left "Type error: expected int in //= at 51:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 8 (Position 52 5)) (Position 52 1),
            FloorDivAssignStmt "x" (IntegerExpr 0 (Position 53 7)) (Position 53 1)
          ]
      )
      `shouldBe` Left "Value error: division by zero at 53:1"
