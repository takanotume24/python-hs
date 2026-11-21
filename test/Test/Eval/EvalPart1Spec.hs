{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.EvalPart1Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, BreakStmt, ContinueStmt, ForStmt, FunctionDefStmt, GlobalStmt, IfStmt, PassStmt, PrintStmt, ReturnStmt, WhileStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "evalProgram - part 1" $ do
  it "prints integer literal" $ do
    evalProgram (Program [PrintStmt (IntegerExpr 42 (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["42"]

  it "prints unary minus integer literal" $ do
    evalProgram (Program [PrintStmt (IntegerExpr (-2) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["-2"]

  it "prints float literals including scientific notation" $ do
    evalProgram (Program [PrintStmt (FloatExpr 1.0 (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1.0"]
    evalProgram (Program [PrintStmt (FloatExpr 0.5 (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0.5"]
    evalProgram (Program [PrintStmt (FloatExpr 1000.0 (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1000.0"]

  it "evaluates unary minus for identifier and expression" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 3 (Position 0 0)) (Position 0 0),
            PrintStmt (UnaryMinusExpr (IdentifierExpr "x" (Position 0 0)) (Position 0 0)) (Position 0 0),
            PrintStmt (UnaryMinusExpr (BinaryExpr AddOperator (IntegerExpr 1 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["-3", "-3"]

  it "prints string literal" $ do
    evalProgram (Program [PrintStmt (StringExpr "hello" (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["hello"]

  it "prints None literal" $ do
    evalProgram (Program [PrintStmt (NoneExpr (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["None"]

  it "evaluates global statement as no-op" $ do
    evalProgram
      ( Program
          [ GlobalStmt "x" (Position 0 0),
            AssignStmt "x" (IntegerExpr 1 (Position 0 0)) (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1"]

  it "evaluates pass statement as no-op" $ do
    evalProgram
      ( Program
          [ PassStmt (Position 0 0),
            AssignStmt "x" (IntegerExpr 2 (Position 0 0)) (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["2"]

  it "assigns string to variable and prints identifier" $ do
    evalProgram (Program [AssignStmt "x" (StringExpr "hello" (Position 0 0)) (Position 0 0), PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["hello"]

  it "concatenates strings with plus" $ do
    evalProgram (Program [PrintStmt (BinaryExpr AddOperator (StringExpr "a" (Position 0 0)) (StringExpr "b" (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["ab"]

  it "evaluates multiplicative operators" $ do
    evalProgram (Program [PrintStmt (BinaryExpr MultiplyOperator (IntegerExpr 6 (Position 0 0)) (IntegerExpr 7 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["42"]
    evalProgram (Program [PrintStmt (BinaryExpr DivideOperator (IntegerExpr 7 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3.5"]
    evalProgram (Program [PrintStmt (BinaryExpr FloorDivideOperator (IntegerExpr 7 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]
    evalProgram (Program [PrintStmt (BinaryExpr ModuloOperator (IntegerExpr 7 (Position 0 0)) (IntegerExpr 4 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]
    evalProgram (Program [PrintStmt (BinaryExpr SubtractOperator (IntegerExpr 7 (Position 0 0)) (IntegerExpr 4 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]
    evalProgram (Program [PrintStmt (BinaryExpr DivideOperator (FloatExpr 7.0 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3.5"]
    evalProgram (Program [PrintStmt (BinaryExpr AddOperator (FloatExpr 1.5 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3.5"]

  it "evaluates len builtin for string" $ do
    evalProgram (Program [PrintStmt (CallExpr "len" [StringExpr "abc" (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]

  it "evaluates len builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "len" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["2"]

  it "evaluates bool builtin truthiness across core types" $ do
    evalProgram (Program [PrintStmt (CallExpr "bool" [IntegerExpr 0 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [FloatExpr 0.0 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [FloatExpr 0.5 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [NoneExpr (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [StringExpr "" (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [StringExpr "x" (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [ListExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [DictExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]

  it "evaluates append builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "append" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2, 3]"]

  it "evaluates sort builtin for numeric list" $ do
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [IntegerExpr 3 (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2, 3]"]
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [FloatExpr 3.2 (Position 0 0), FloatExpr 1.1 (Position 0 0), FloatExpr 2.4 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1.1, 2.4, 3.2]"]
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [IntegerExpr 3 (Position 0 0), FloatExpr 1.5 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1.5, 2, 3]"]
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[]"]

  it "evaluates reverse builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "reverse" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[3, 2, 1]"]
    evalProgram (Program [PrintStmt (CallExpr "reverse" [ListExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[]"]

  it "reports reverse builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "reverse" [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: reverse expects list as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "reverse" [ListExpr [] (Position 0 0), IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling reverse at 0:0"

  it "reports sort builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "sort" [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: sort expects list as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [StringExpr "x" (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: sort expects list of number at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [] (Position 0 0), IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling sort at 0:0"

  it "evaluates remove builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "remove" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2]"]

  it "reports remove builtin type/value/argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "remove" [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: remove expects list as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "remove" [ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0), IntegerExpr 9 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Value error: remove value not found at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "remove" [ListExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling remove at 0:0"

  it "evaluates insert builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "insert" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2, 3]"]
    evalProgram (Program [PrintStmt (CallExpr "insert" [ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0), IntegerExpr (-1) (Position 0 0), IntegerExpr 0 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[0, 1]"]
    evalProgram (Program [PrintStmt (CallExpr "insert" [ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0), IntegerExpr 9 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2]"]

  it "reports insert builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "insert" [IntegerExpr 1 (Position 0 0), IntegerExpr 0 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: insert expects list as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "insert" [ListExpr [] (Position 0 0), StringExpr "x" (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: insert expects int index at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "insert" [ListExpr [] (Position 0 0), IntegerExpr 0 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling insert at 0:0"

  it "evaluates pop builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "pop" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["2"]

  it "evaluates pop builtin for dictionary with optional default" $ do
    evalProgram (Program [PrintStmt (CallExpr "pop" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 4 (Position 0 0)}] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["4"]
    evalProgram (Program [PrintStmt (CallExpr "pop" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 9 (Position 0 0), IntegerExpr 99 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["99"]

  it "evaluates clear builtin for list and dictionary" $ do
    evalProgram (Program [PrintStmt (CallExpr "clear" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[]"]
    evalProgram (Program [PrintStmt (CallExpr "clear" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{}"]

  it "reports clear builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "clear" [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: clear expects list or dict at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "clear" [ListExpr [] (Position 0 0), IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling clear at 0:0"

  it "evaluates keys builtin for dictionary" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "keys" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 4 (Position 0 0)}] (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["[1, 3]"]

  it "evaluates get builtin for dictionary" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "get" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 4 (Position 0 0)}] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["4"]

  it "evaluates get builtin with default value" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "get" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 9 (Position 0 0), IntegerExpr 99 (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "get" [DictExpr [DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 2 (Position 0 0)}] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 99 (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["99", "2"]
