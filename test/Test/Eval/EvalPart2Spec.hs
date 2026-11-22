{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.EvalPart2Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, BreakStmt, ContinueStmt, ForStmt, FunctionDefStmt, GlobalStmt, IfStmt, PassStmt, PrintStmt, ReturnStmt, WhileStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "evalProgram - part 2" $ do
  it "evaluates update builtin for dictionary" $ do
    evalProgram (Program [PrintStmt (CallExpr "update" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 9 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 9}"]
    evalProgram (Program [PrintStmt (CallExpr "update" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: 4}"]

  it "evaluates setdefault builtin for dictionary" $ do
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 9 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2}"]
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: 4}"]

  it "evaluates setdefault builtin with omitted default as None" $ do
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: None}"]

  it "reports setdefault builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: setdefault expects dict as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: setdefault expects dict as first argument at 0:0"

  it "evaluates range builtin with start/stop and step" $ do
    evalProgram (Program [PrintStmt (CallExpr "range" [IntegerExpr 2 (Position 0 0), IntegerExpr 5 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[2, 3, 4]"]
    evalProgram (Program [PrintStmt (CallExpr "range" [IntegerExpr 5 (Position 0 0), IntegerExpr 0 (Position 0 0), IntegerExpr (-2) (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[5, 3, 1]"]

  it "evaluates values and items builtins for dictionary" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "values" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 4 (Position 0 0)}] (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "items" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 4 (Position 0 0)}] (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["[2, 4]", "[[1, 2], [3, 4]]"]

  it "preserves insertion order for dictionary builtins" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "keys" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 30 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 10 (Position 0 0)}] (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "values" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 30 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 10 (Position 0 0)}] (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "items" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 30 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 10 (Position 0 0)}] (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["[3, 1]", "[30, 10]", "[[3, 30], [1, 10]]"]

  it "evaluates string equality and inequality" $ do
    evalProgram (Program [PrintStmt (BinaryExpr EqOperator (StringExpr "a" (Position 0 0)) (StringExpr "a" (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (BinaryExpr NotEqOperator (StringExpr "a" (Position 0 0)) (StringExpr "b" (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]

  it "evaluates None equality and inequality" $ do
    evalProgram (Program [PrintStmt (BinaryExpr EqOperator (NoneExpr (Position 0 0)) (NoneExpr (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (BinaryExpr NotEqOperator (NoneExpr (Position 0 0)) (NoneExpr (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]

  it "evaluates mixed int/float equality and comparisons" $ do
    evalProgram (Program [PrintStmt (BinaryExpr EqOperator (IntegerExpr 1 (Position 0 0)) (FloatExpr 1.0 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (BinaryExpr LtOperator (IntegerExpr 1 (Position 0 0)) (FloatExpr 2.0 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]

  it "treats None as falsy in if/while/not" $ do
    evalProgram
      ( Program
          [ IfStmt
              (NoneExpr (Position 0 0))
              [PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)]
              (Just [PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0)])
              (Position 0 0),
            AssignStmt "x" (IntegerExpr 0 (Position 0 0)) (Position 0 0),
            WhileStmt
              (NoneExpr (Position 0 0))
              [AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 1 (Position 0 0)) (Position 0 0)) (Position 0 0)]
              Nothing
              (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0),
            PrintStmt (NotExpr (NoneExpr (Position 0 0)) (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["2", "0", "1"]

  it "treats string/list/dict values as truthy or falsy in if/not" $ do
    evalProgram
      ( Program
          [ IfStmt
              (StringExpr "" (Position 0 0))
              [PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)]
              (Just [PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0)])
              (Position 0 0),
            IfStmt
              (ListExpr [] (Position 0 0))
              [PrintStmt (IntegerExpr 3 (Position 0 0)) (Position 0 0)]
              (Just [PrintStmt (IntegerExpr 4 (Position 0 0)) (Position 0 0)])
              (Position 0 0),
            IfStmt
              (DictExpr [] (Position 0 0))
              [PrintStmt (IntegerExpr 5 (Position 0 0)) (Position 0 0)]
              (Just [PrintStmt (IntegerExpr 6 (Position 0 0)) (Position 0 0)])
              (Position 0 0),
            PrintStmt (NotExpr (StringExpr "" (Position 0 0)) (Position 0 0)) (Position 0 0),
            PrintStmt (NotExpr (ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["2", "4", "6", "1", "0"]

  it "allows pass inside if/else" $ do
    evalProgram
      ( Program
          [ IfStmt
              (IntegerExpr 0 (Position 0 0))
              [PassStmt (Position 0 0)]
              (Just [PassStmt (Position 0 0)])
              (Position 0 0),
            PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1"]

  it "allows pass inside while body" $ do
    evalProgram
      ( Program
          [ WhileStmt
              (IntegerExpr 0 (Position 0 0))
              [PassStmt (Position 0 0)]
              Nothing
              (Position 0 0),
            PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1"]

  it "allows pass inside for body" $ do
    evalProgram
      ( Program
          [ ForStmt
              "i"
              (ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0))
              [PassStmt (Position 0 0)]
              Nothing
              (Position 0 0),
            PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["2"]

  it "prints list literal" $ do
    evalProgram (Program [PrintStmt (ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2]"]

  it "prints dictionary literal" $ do
    evalProgram (Program [PrintStmt (DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 4 (Position 0 0)}] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: 4}"]

  it "collects multiple prints" $ do
    evalProgram (Program [PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0), PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1", "2"]

  it "evaluates assignment and uses identifier in expression" $ do
    evalProgram (Program [AssignStmt "x" (IntegerExpr 3 (Position 0 0)) (Position 0 0), PrintStmt (BinaryExpr AddOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["5"]

  it "returns no output for program with only assignment" $ do
    evalProgram (Program [AssignStmt "x" (IntegerExpr 10 (Position 0 0)) (Position 0 0)]) `shouldBe` Right []

  it "calls function and collects prints and return value" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "add" ["a", "b"] [PrintStmt (IdentifierExpr "a" (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1", "0"]

  it "errors on undefined function call" $ do
    evalProgram (Program [PrintStmt (CallExpr "nope" [] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Name error: undefined function nope at 0:0"

  it "errors on undefined identifier (with position)" $ do
    evalProgram (Program [PrintStmt (IdentifierExpr "x" (Position 1 2)) (Position 1 2)]) `shouldBe` Left "Name error: undefined identifier x at 1:2"
