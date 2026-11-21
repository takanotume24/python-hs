{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.RuntimeErrorPart7Spec (spec) where

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
spec = describe "runtime error reporting - part 7" $ do
  it "reports dictionary builtin type and value errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "keys" [IntegerExpr 1 (Position 21 12)] (Position 21 7)) (Position 21 1)
          ]
      )
      `shouldBe` Left "Type error: keys expects dict at 21:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "get" [IntegerExpr 1 (Position 23 11), IntegerExpr 2 (Position 23 14)] (Position 23 7)) (Position 23 1)
          ]
      )
      `shouldBe` Left "Type error: get expects dict as first argument at 23:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "get" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 24 13), dictEntryValue = IntegerExpr 2 (Position 24 16)}] (Position 24 12), IntegerExpr 3 (Position 24 20)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Key not found in get at 24:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "values" [IntegerExpr 1 (Position 25 14)] (Position 25 7)) (Position 25 1)
          ]
      )
      `shouldBe` Left "Type error: values expects dict at 25:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "update" [IntegerExpr 1 (Position 27 14), IntegerExpr 2 (Position 27 17), IntegerExpr 3 (Position 27 20)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Type error: update expects dict as first argument at 27:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "update" [DictExpr [] (Position 28 14), IntegerExpr 2 (Position 28 17)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Type error: update expects dict as second argument at 28:7"

  it "reports dictionary builtin argument count mismatches" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "keys" [] (Position 22 7)) (Position 22 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling keys at 22:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "get" [DictExpr [] (Position 24 12)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling get at 24:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "items" [] (Position 26 7)) (Position 26 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling items at 26:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "update" [DictExpr [] (Position 29 14)] (Position 29 7)) (Position 29 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling update at 29:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "setdefault" [DictExpr [] (Position 30 18)] (Position 30 7)) (Position 30 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling setdefault at 30:7"

  it "reports extended range builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "range" [IntegerExpr 1 (Position 27 13), IntegerExpr 5 (Position 27 16), IntegerExpr 0 (Position 27 19)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Value error: range step must not be zero at 27:7"

  it "enforces iteration limit boundary for while and for loops" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 27 1)) (Position 27 1),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 28 7)) (IntegerExpr 2000 (Position 28 11)) (Position 28 9))
              [AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 29 5)) (IntegerExpr 1 (Position 29 9)) (Position 29 7)) (Position 29 1)]
              (Position 28 1),
            PrintStmt (IdentifierExpr "x" (Position 30 7)) (Position 30 1)
          ]
      )
      `shouldBe` Right ["2000"]

    evalProgram
      ( Program
          [ ForStmt
              "i"
              (CallExpr "range" [IntegerExpr 2000 (Position 31 17)] (Position 31 10))
              [PassStmt (Position 32 1)]
              (Position 31 1),
            PrintStmt (IntegerExpr 1 (Position 33 7)) (Position 33 1)
          ]
      )
      `shouldBe` Right ["1"]

    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 34 1)) (Position 34 1),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 35 7)) (IntegerExpr 2001 (Position 35 11)) (Position 35 9))
              [AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 36 5)) (IntegerExpr 1 (Position 36 9)) (Position 36 7)) (Position 36 1)]
              (Position 35 1)
          ]
      )
      `shouldBe` Left "Value error: iteration limit exceeded at 35:1"

    evalProgram
      ( Program
          [ ForStmt
              "i"
              (CallExpr "range" [IntegerExpr 2001 (Position 37 17)] (Position 37 10))
              [PassStmt (Position 38 1)]
              (Position 37 1)
          ]
      )
      `shouldBe` Left "Value error: iteration limit exceeded at 37:1"

  it "reports for-loop iterable type error clearly" $ do
    evalProgram
      ( Program
          [ ForStmt "i" (StringExpr "abc" (Position 21 10)) [PrintStmt (IdentifierExpr "i" (Position 21 22)) (Position 21 16)] (Position 21 1)
          ]
      )
      `shouldBe` Left "Type error: for expects iterable (int range, list, or dict) at 21:10"

  it "reports unary minus type errors clearly" $ do
    evalProgram
      ( Program
          [ PrintStmt (UnaryMinusExpr (StringExpr "x" (Position 22 8)) (Position 22 7)) (Position 22 1)
          ]
      )
      `shouldBe` Left "Type error: unary - expects int at 22:7"

    evalProgram
      ( Program
          [ PrintStmt (UnaryMinusExpr (ListExpr [] (Position 23 8)) (Position 23 7)) (Position 23 1)
          ]
      )
      `shouldBe` Left "Type error: unary - expects int at 23:7"
