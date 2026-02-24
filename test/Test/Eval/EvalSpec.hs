module Test.Eval.EvalSpec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, BreakStmt, ContinueStmt, ForStmt, FunctionDefStmt, GlobalStmt, IfStmt, PassStmt, PrintStmt, ReturnStmt, WhileStmt))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Evaluator (evalProgram)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "evalProgram" $ do
  it "prints integer literal" $ do
    evalProgram (Program [PrintStmt (IntegerExpr 42 (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["42"]

  it "prints unary minus integer literal" $ do
    evalProgram (Program [PrintStmt (IntegerExpr (-2) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["-2"]

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
    evalProgram (Program [PrintStmt (BinaryExpr DivideOperator (IntegerExpr 7 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]
    evalProgram (Program [PrintStmt (BinaryExpr ModuloOperator (IntegerExpr 7 (Position 0 0)) (IntegerExpr 4 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]

  it "evaluates len builtin for string" $ do
    evalProgram (Program [PrintStmt (CallExpr "len" [StringExpr "abc" (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["3"]

  it "evaluates len builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "len" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["2"]

  it "evaluates bool builtin truthiness across core types" $ do
    evalProgram (Program [PrintStmt (CallExpr "bool" [IntegerExpr 0 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [NoneExpr (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [StringExpr "" (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [StringExpr "x" (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [ListExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [ListExpr [IntegerExpr 1 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [DictExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]
    evalProgram (Program [PrintStmt (CallExpr "bool" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]

  it "evaluates append builtin for list" $ do
    evalProgram (Program [PrintStmt (CallExpr "append" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2, 3]"]

  it "evaluates sort builtin for integer list" $ do
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [IntegerExpr 3 (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2, 3]"]
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[]"]

  it "reports sort builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "sort" [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: sort expects list as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "sort" [ListExpr [StringExpr "x" (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: sort expects list of int at 0:0"
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
    evalProgram (Program [PrintStmt (CallExpr "pop" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)), (IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0))] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["4"]
    evalProgram (Program [PrintStmt (CallExpr "pop" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 9 (Position 0 0), IntegerExpr 99 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["99"]

  it "evaluates clear builtin for list and dictionary" $ do
    evalProgram (Program [PrintStmt (CallExpr "clear" [ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[]"]
    evalProgram (Program [PrintStmt (CallExpr "clear" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{}"]

  it "reports clear builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "clear" [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: clear expects list or dict at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "clear" [ListExpr [] (Position 0 0), IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling clear at 0:0"

  it "evaluates keys builtin for dictionary" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "keys" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)), (IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0))] (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["[1, 3]"]

  it "evaluates get builtin for dictionary" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "get" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)), (IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0))] (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["4"]

  it "evaluates get builtin with default value" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "get" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 9 (Position 0 0), IntegerExpr 99 (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "get" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 99 (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["99", "2"]

  it "evaluates update builtin for dictionary" $ do
    evalProgram (Program [PrintStmt (CallExpr "update" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 9 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 9}"]
    evalProgram (Program [PrintStmt (CallExpr "update" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: 4}"]

  it "evaluates setdefault builtin for dictionary" $ do
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 1 (Position 0 0), IntegerExpr 9 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2}"]
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0))] (Position 0 0), IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: 4}"]

  it "reports setdefault builtin type and argument errors" $ do
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0), IntegerExpr 3 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Type error: setdefault expects dict as first argument at 0:0"
    evalProgram (Program [PrintStmt (CallExpr "setdefault" [DictExpr [] (Position 0 0), IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Argument count mismatch when calling setdefault at 0:0"

  it "evaluates range builtin with start/stop and step" $ do
    evalProgram (Program [PrintStmt (CallExpr "range" [IntegerExpr 2 (Position 0 0), IntegerExpr 5 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[2, 3, 4]"]
    evalProgram (Program [PrintStmt (CallExpr "range" [IntegerExpr 5 (Position 0 0), IntegerExpr 0 (Position 0 0), IntegerExpr (-2) (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[5, 3, 1]"]

  it "evaluates values and items builtins for dictionary" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "values" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)), (IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0))] (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "items" [DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)), (IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0))] (Position 0 0)] (Position 0 0))
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["[2, 4]", "[[1, 2], [3, 4]]"]

  it "preserves insertion order for dictionary builtins" $ do
    evalProgram
      ( Program
          [ PrintStmt
              (CallExpr "keys" [DictExpr [(IntegerExpr 3 (Position 0 0), IntegerExpr 30 (Position 0 0)), (IntegerExpr 1 (Position 0 0), IntegerExpr 10 (Position 0 0))] (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "values" [DictExpr [(IntegerExpr 3 (Position 0 0), IntegerExpr 30 (Position 0 0)), (IntegerExpr 1 (Position 0 0), IntegerExpr 10 (Position 0 0))] (Position 0 0)] (Position 0 0))
              (Position 0 0),
            PrintStmt
              (CallExpr "items" [DictExpr [(IntegerExpr 3 (Position 0 0), IntegerExpr 30 (Position 0 0)), (IntegerExpr 1 (Position 0 0), IntegerExpr 10 (Position 0 0))] (Position 0 0)] (Position 0 0))
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
              (Position 0 0),
            PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["2"]

  it "prints list literal" $ do
    evalProgram (Program [PrintStmt (ListExpr [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["[1, 2]"]

  it "prints dictionary literal" $ do
    evalProgram (Program [PrintStmt (DictExpr [(IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)), (IntegerExpr 3 (Position 0 0), IntegerExpr 4 (Position 0 0))] (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["{1: 2, 3: 4}"]

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

  it "errors on argument count mismatch" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [PrintStmt (IdentifierExpr "a" (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 0 0)] (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling f at 0:0"

  it "keeps outer scope unchanged by function" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 1 (Position 0 0)) (Position 0 0),
            FunctionDefStmt "f" ["y"] [AssignStmt "x" (IntegerExpr 2 (Position 0 0)) (Position 0 0), PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0),
            PrintStmt (CallExpr "f" [IntegerExpr 0 (Position 0 0)] (Position 0 0)) (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1", "2", "0", "1"]

  it "captures prints from nested function calls" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "g" [] [PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0)] (Position 0 0),
            FunctionDefStmt "f" [] [PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0), PrintStmt (CallExpr "g" [] (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (CallExpr "f" [] (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1", "2", "0", "0"]

  it "evaluates comparisons to 1/0" $ do
    evalProgram (Program [PrintStmt (BinaryExpr EqOperator (IntegerExpr 1 (Position 0 0)) (IntegerExpr 1 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (BinaryExpr LtOperator (IntegerExpr 1 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (BinaryExpr GtOperator (IntegerExpr 1 (Position 0 0)) (IntegerExpr 2 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]

  it "evaluates not to boolean-int" $ do
    evalProgram (Program [PrintStmt (NotExpr (IntegerExpr 0 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["1"]
    evalProgram (Program [PrintStmt (NotExpr (IntegerExpr 1 (Position 0 0)) (Position 0 0)) (Position 0 0)]) `shouldBe` Right ["0"]

  it "short-circuits and / or" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "p" [] [PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0), ReturnStmt (IntegerExpr 0 (Position 0 0)) (Position 0 0)] (Position 0 0),
            FunctionDefStmt "q" [] [PrintStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0), ReturnStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (BinaryExpr AndOperator (CallExpr "p" [] (Position 0 0)) (CallExpr "q" [] (Position 0 0)) (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1", "0"]

    evalProgram
      ( Program
          [ FunctionDefStmt "p" [] [PrintStmt (IntegerExpr 3 (Position 0 0)) (Position 0 0), ReturnStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)] (Position 0 0),
            FunctionDefStmt "q" [] [PrintStmt (IntegerExpr 4 (Position 0 0)) (Position 0 0), ReturnStmt (IntegerExpr 0 (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (BinaryExpr OrOperator (CallExpr "p" [] (Position 0 0)) (CallExpr "q" [] (Position 0 0)) (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["3", "1"]

  it "function returns computed value" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "add" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 0 0)) (IdentifierExpr "b" (Position 0 0)) (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (CallExpr "add" [IntegerExpr 1 (Position 0 0), IntegerExpr 2 (Position 0 0)] (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["3"]

  it "return stops function body and returns value" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" [] [PrintStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0), ReturnStmt (IntegerExpr 2 (Position 0 0)) (Position 0 0), PrintStmt (IntegerExpr 3 (Position 0 0)) (Position 0 0)] (Position 0 0),
            PrintStmt (CallExpr "f" [] (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1", "2"]

  it "errors on return outside function" $ do
    evalProgram (Program [ReturnStmt (IntegerExpr 1 (Position 0 0)) (Position 0 0)]) `shouldBe` Left "Return outside function at 0:0"

  it "executes then-branch for truthy if condition" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 1 (Position 0 0)) (Position 0 0),
            IfStmt (IdentifierExpr "x" (Position 0 0)) [PrintStmt (IntegerExpr 10 (Position 0 0)) (Position 0 0)] (Just [PrintStmt (IntegerExpr 20 (Position 0 0)) (Position 0 0)]) (Position 0 0)
          ]
      )
      `shouldBe` Right ["10"]

  it "executes else-branch for falsy if condition" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 0 0)) (Position 0 0),
            IfStmt (IdentifierExpr "x" (Position 0 0)) [PrintStmt (IntegerExpr 10 (Position 0 0)) (Position 0 0)] (Just [PrintStmt (IntegerExpr 20 (Position 0 0)) (Position 0 0)]) (Position 0 0)
          ]
      )
      `shouldBe` Right ["20"]

  it "evaluates while loop until condition becomes false" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 0 0)) (Position 0 0),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 3 (Position 0 0)) (Position 0 0))
              [ PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0),
                AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 1 (Position 0 0)) (Position 0 0)) (Position 0 0)
              ]
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["0", "1", "2"]

  it "supports break and continue in while loop" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 0 0)) (Position 0 0),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 5 (Position 0 0)) (Position 0 0))
              [ AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 1 (Position 0 0)) (Position 0 0)) (Position 0 0),
                ContinueStmt (Position 0 0),
                PrintStmt (IntegerExpr 999 (Position 0 0)) (Position 0 0)
              ]
              (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["5"]

    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 0 0)) (Position 0 0),
            WhileStmt
              (IntegerExpr 1 (Position 0 0))
              [ AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 0 0)) (IntegerExpr 1 (Position 0 0)) (Position 0 0)) (Position 0 0),
                BreakStmt (Position 0 0)
              ]
              (Position 0 0),
            PrintStmt (IdentifierExpr "x" (Position 0 0)) (Position 0 0)
          ]
      )
      `shouldBe` Right ["1"]

  it "supports for loop with range builtin" $ do
    evalProgram
      ( Program
          [ ForStmt "i" (CallExpr "range" [IntegerExpr 3 (Position 0 0)] (Position 0 0)) [PrintStmt (IdentifierExpr "i" (Position 0 0)) (Position 0 0)] (Position 0 0)
          ]
      )
      `shouldBe` Right ["0", "1", "2"]

  it "supports for loop over list literal" $ do
    evalProgram
      ( Program
          [ ForStmt
              "i"
              (ListExpr [IntegerExpr 10 (Position 0 0), IntegerExpr 20 (Position 0 0)] (Position 0 0))
              [PrintStmt (IdentifierExpr "i" (Position 0 0)) (Position 0 0)]
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["10", "20"]

  it "supports for loop over dictionary keys" $ do
    evalProgram
      ( Program
          [ ForStmt
              "k"
              (DictExpr [(IntegerExpr 3 (Position 0 0), IntegerExpr 30 (Position 0 0)), (IntegerExpr 1 (Position 0 0), IntegerExpr 10 (Position 0 0))] (Position 0 0))
              [PrintStmt (IdentifierExpr "k" (Position 0 0)) (Position 0 0)]
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["3", "1"]
