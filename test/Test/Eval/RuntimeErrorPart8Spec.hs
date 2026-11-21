{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart8Spec (spec) where

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
spec = describe "runtime error reporting - part 8" $ do
  it "reports pop builtin type and value errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [ListExpr [] (Position 24 10)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Value error: pop from empty list at 24:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [IntegerExpr 1 (Position 25 11)] (Position 25 7)) (Position 25 1)
          ]
      )
      `shouldBe` Left "Type error: pop expects list at 25:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 27 11), dictEntryValue = IntegerExpr 2 (Position 27 14)}] (Position 27 10), IntegerExpr 9 (Position 27 18)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Key not found in pop at 27:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [IntegerExpr 1 (Position 28 11), IntegerExpr 2 (Position 28 14)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Type error: pop expects dict as first argument at 28:7"

  it "reports pop builtin argument count mismatches" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [ListExpr [IntegerExpr 1 (Position 26 11)] (Position 26 10), IntegerExpr 0 (Position 26 14)] (Position 26 7)) (Position 26 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling pop at 26:7"

  it "reports clear builtin type errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "clear" [IntegerExpr 1 (Position 27 11)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Type error: clear expects list or dict at 27:7"

  it "reports clear builtin argument count mismatches" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "clear" [ListExpr [IntegerExpr 1 (Position 28 11)] (Position 28 10), IntegerExpr 0 (Position 28 14)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling clear at 28:7"

  it "reports setdefault builtin type errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "setdefault" [IntegerExpr 1 (Position 29 11), IntegerExpr 2 (Position 29 14), IntegerExpr 3 (Position 29 17)] (Position 29 7)) (Position 29 1)
          ]
      )
      `shouldBe` Left "Type error: setdefault expects dict as first argument at 29:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "setdefault" [IntegerExpr 1 (Position 30 11), IntegerExpr 2 (Position 30 14)] (Position 30 7)) (Position 30 1)
          ]
      )
      `shouldBe` Left "Type error: setdefault expects dict as first argument at 30:7"

  it "reports insert builtin type errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "insert" [IntegerExpr 1 (Position 31 11), IntegerExpr 0 (Position 31 14), IntegerExpr 2 (Position 31 17)] (Position 31 7)) (Position 31 1)
          ]
      )
      `shouldBe` Left "Type error: insert expects list as first argument at 31:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "insert" [ListExpr [] (Position 32 10), StringExpr "x" (Position 32 14), IntegerExpr 2 (Position 32 19)] (Position 32 7)) (Position 32 1)
          ]
      )
      `shouldBe` Left "Type error: insert expects int index at 32:7"

  it "reports insert builtin argument count mismatches" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "insert" [ListExpr [] (Position 33 10), IntegerExpr 0 (Position 33 14)] (Position 33 7)) (Position 33 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling insert at 33:7"

  it "reports remove builtin type and value errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "remove" [IntegerExpr 1 (Position 34 11), IntegerExpr 2 (Position 34 14)] (Position 34 7)) (Position 34 1)
          ]
      )
      `shouldBe` Left "Type error: remove expects list as first argument at 34:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "remove" [ListExpr [IntegerExpr 1 (Position 35 11)] (Position 35 10), IntegerExpr 2 (Position 35 14)] (Position 35 7)) (Position 35 1)
          ]
      )
      `shouldBe` Left "Value error: remove value not found at 35:7"

  it "reports remove builtin argument count mismatches" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "remove" [ListExpr [IntegerExpr 1 (Position 36 11)] (Position 36 10)] (Position 36 7)) (Position 36 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling remove at 36:7"

  it "reports plus-assign errors" $ do
    evalProgram
      ( Program
          [ AddAssignStmt "x" (IntegerExpr 1 (Position 30 6)) (Position 30 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier x at 30:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (ListExpr [] (Position 31 5)) (Position 31 1),
            AddAssignStmt "x" (IntegerExpr 1 (Position 32 6)) (Position 32 1)
          ]
      )
      `shouldBe` Left "Type error: + expects int+int or string+string at 32:1"

  it "reports minus-assign errors" $ do
    evalProgram
      ( Program
          [ SubAssignStmt "x" (IntegerExpr 1 (Position 33 6)) (Position 33 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier x at 33:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (StringExpr "a" (Position 34 5)) (Position 34 1),
            SubAssignStmt "x" (IntegerExpr 1 (Position 35 6)) (Position 35 1)
          ]
      )
      `shouldBe` Left "Type error: expected int in -= at 35:1"

  it "reports star-assign errors" $ do
    evalProgram
      ( Program
          [ MulAssignStmt "x" (IntegerExpr 2 (Position 36 6)) (Position 36 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier x at 36:1"

    evalProgram
      ( Program
          [ AssignStmt "x" (StringExpr "a" (Position 37 5)) (Position 37 1),
            MulAssignStmt "x" (IntegerExpr 2 (Position 38 6)) (Position 38 1)
          ]
      )
      `shouldBe` Left "Type error: expected int in *= at 38:1"
