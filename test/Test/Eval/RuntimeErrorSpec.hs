module Test.Eval.RuntimeErrorSpec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, MultiplyOperator, DivideOperator, ModuloOperator, LtOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr, NoneExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FunctionDefStmt, IfStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runtime error reporting" $ do
  it "reports position for undefined identifier in if condition" $ do
    evalProgram
      ( Program
          [ IfStmt
              (IdentifierExpr "missing" (Position 3 8))
              [PrintStmt (IntegerExpr 1 (Position 3 10)) (Position 3 10)]
              Nothing
              (Position 3 1)
          ]
      )
        `shouldBe` Left "Name error: undefined identifier missing at 3:8"

  it "reports position for undefined identifier in while condition" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 5 1)) (Position 5 1),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 6 7)) (IdentifierExpr "limit" (Position 6 11)) (Position 6 9))
              [PrintStmt (IdentifierExpr "x" (Position 7 9)) (Position 7 9)]
              (Position 6 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier limit at 6:11"

  it "reports position for return outside function" $ do
    evalProgram
      ( Program
          [ ReturnStmt (IntegerExpr 99 (Position 10 10)) (Position 10 3)
          ]
      )
      `shouldBe` Left "Return outside function at 10:3"

  it "reports position for undefined identifier read inside function" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "readMissing" [] [ReturnStmt (IdentifierExpr "missing" (Position 11 15)) (Position 11 8)] (Position 11 1),
            PrintStmt (CallExpr "readMissing" [] (Position 12 1)) (Position 12 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 11:15"

  it "prioritizes function call argument errors at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 2 10)) (Position 2 3)] (Position 1 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "b" (IntegerExpr 1 (Position 3 13)) (Position 3 11),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 3 18)) (Position 3 16)
                  ]
                  (Position 3 7)
              )
              (Position 3 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument b at 3:16"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 5 10)) (Position 5 3)] (Position 4 1),
            PrintStmt
              (CallExpr "f" [IntegerExpr 1 (Position 6 9), KeywordArgExpr "b" (IntegerExpr 2 (Position 6 16)) (Position 6 14)] (Position 6 7))
              (Position 6 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument b at 6:14"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 8 10)) (Position 8 3)] (Position 7 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 9 9),
                    IntegerExpr 2 (Position 9 12),
                    KeywordArgExpr "a" (IntegerExpr 3 (Position 9 20)) (Position 9 18)
                  ]
                  (Position 9 7)
              )
              (Position 9 1)
          ]
      )
      `shouldBe` Left "Argument error: multiple values for parameter a at 9:18"

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

  it "reports type error for len on unsupported argument" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "len" [IntegerExpr 1 (Position 13 9)] (Position 13 7)) (Position 13 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 13:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "len" [NoneExpr (Position 13 9)] (Position 13 7)) (Position 13 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 13:7"

  it "reports argument count mismatch for len" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "len" [] (Position 14 7)) (Position 14 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling len at 14:7"

  it "reports builtin keyword argument as unsupported at evaluator layer" $ do
    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "len"
                  [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 15 13)] (Position 15 12)) (Position 15 11)]
                  (Position 15 7)
              )
              (Position 15 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 15:11"

  it "reports method-style builtin keyword argument as unsupported at evaluator layer" $ do
    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "update"
                  [ DictExpr [] (Position 16 9),
                    KeywordArgExpr "k" (IntegerExpr 1 (Position 16 22)) (Position 16 20)
                  ]
                  (Position 16 7)
              )
              (Position 16 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 16:20"

  it "reports bool builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "bool" [] (Position 14 7)) (Position 14 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling bool at 14:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "bool" [IntegerExpr 1 (Position 15 12), IntegerExpr 2 (Position 15 15)] (Position 15 7)) (Position 15 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling bool at 15:7"

  it "reports break/continue outside loop" $ do
    evalProgram (Program [BreakStmt (Position 15 3)]) `shouldBe` Left "Break outside loop at 15:3"
    evalProgram (Program [ContinueStmt (Position 16 3)]) `shouldBe` Left "Continue outside loop at 16:3"

  it "reports range builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "range" [StringExpr "x" (Position 17 13)] (Position 17 7)) (Position 17 1)
          ]
      )
      `shouldBe` Left "Type error: range expects int at 17:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "range" [] (Position 18 7)) (Position 18 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling range at 18:7"

  it "reports append builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "append" [IntegerExpr 1 (Position 19 14), IntegerExpr 2 (Position 19 17)] (Position 19 7)) (Position 19 1)
          ]
      )
      `shouldBe` Left "Type error: append expects list as first argument at 19:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "append" [ListExpr [IntegerExpr 1 (Position 20 15)] (Position 20 14)] (Position 20 7)) (Position 20 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling append at 20:7"

  it "reports dictionary builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "keys" [IntegerExpr 1 (Position 21 12)] (Position 21 7)) (Position 21 1)
          ]
      )
      `shouldBe` Left "Type error: keys expects dict at 21:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "keys" [] (Position 22 7)) (Position 22 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling keys at 22:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "get" [IntegerExpr 1 (Position 23 11), IntegerExpr 2 (Position 23 14)] (Position 23 7)) (Position 23 1)
          ]
      )
      `shouldBe` Left "Type error: get expects dict as first argument at 23:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "get" [DictExpr [(IntegerExpr 1 (Position 24 13), IntegerExpr 2 (Position 24 16))] (Position 24 12), IntegerExpr 3 (Position 24 20)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Key not found in get at 24:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "get" [DictExpr [] (Position 24 12)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling get at 24:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "values" [IntegerExpr 1 (Position 25 14)] (Position 25 7)) (Position 25 1)
          ]
      )
      `shouldBe` Left "Type error: values expects dict at 25:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "items" [] (Position 26 7)) (Position 26 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling items at 26:7"

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

  it "reports extended range builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "range" [IntegerExpr 1 (Position 27 13), IntegerExpr 5 (Position 27 16), IntegerExpr 0 (Position 27 19)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Value error: range step must not be zero at 27:7"

  it "reports iteration limit exceeded for while and for loops" $ do
    evalProgram
      ( Program
          [ AssignStmt "x" (IntegerExpr 0 (Position 28 1)) (Position 28 1),
            WhileStmt
              (BinaryExpr LtOperator (IdentifierExpr "x" (Position 29 7)) (IntegerExpr 10001 (Position 29 11)) (Position 29 9))
              [AssignStmt "x" (BinaryExpr AddOperator (IdentifierExpr "x" (Position 30 5)) (IntegerExpr 1 (Position 30 9)) (Position 30 7)) (Position 30 1)]
              (Position 29 1)
          ]
      )
      `shouldBe` Left "Value error: iteration limit exceeded at 29:1"

    evalProgram
      ( Program
          [ ForStmt
              "i"
              (CallExpr "range" [IntegerExpr 10001 (Position 31 17)] (Position 31 10))
              [PassStmt (Position 32 1)]
              (Position 31 1)
          ]
      )
      `shouldBe` Left "Value error: iteration limit exceeded at 31:1"

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

  it "reports pop builtin errors" $ do
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
          [ PrintStmt (CallExpr "pop" [ListExpr [IntegerExpr 1 (Position 26 11)] (Position 26 10), IntegerExpr 0 (Position 26 14)] (Position 26 7)) (Position 26 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling pop at 26:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [DictExpr [(IntegerExpr 1 (Position 27 11), IntegerExpr 2 (Position 27 14))] (Position 27 10), IntegerExpr 9 (Position 27 18)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Key not found in pop at 27:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "pop" [IntegerExpr 1 (Position 28 11), IntegerExpr 2 (Position 28 14)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Type error: pop expects dict as first argument at 28:7"

  it "reports clear builtin errors" $ do
    evalProgram
      ( Program
          [ PrintStmt (CallExpr "clear" [IntegerExpr 1 (Position 27 11)] (Position 27 7)) (Position 27 1)
          ]
      )
      `shouldBe` Left "Type error: clear expects list or dict at 27:7"

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "clear" [ListExpr [IntegerExpr 1 (Position 28 11)] (Position 28 10), IntegerExpr 0 (Position 28 14)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling clear at 28:7"

  it "reports setdefault builtin errors" $ do
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

  it "reports insert builtin errors" $ do
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

    evalProgram
      ( Program
          [ PrintStmt (CallExpr "insert" [ListExpr [] (Position 33 10), IntegerExpr 0 (Position 33 14)] (Position 33 7)) (Position 33 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling insert at 33:7"

  it "reports remove builtin errors" $ do
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
