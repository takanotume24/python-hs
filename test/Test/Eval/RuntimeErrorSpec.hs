module Test.Eval.RuntimeErrorSpec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (AddOperator, MultiplyOperator, DivideOperator, ModuloOperator, LtOperator))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, ListExpr, NoneExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, IfStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
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
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 7 10)) (Position 7 3)] (Position 7 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 8 9),
                    KeywordArgExpr "z" (IntegerExpr 2 (Position 8 14)) (Position 8 12),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 8 21)) (Position 8 19)
                  ]
                  (Position 8 7)
              )
              (Position 8 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument z at 8:12"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 9 10)) (Position 9 3)] (Position 9 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "z" (IntegerExpr 1 (Position 10 11)) (Position 10 9),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 10 16)) (Position 10 14)
                  ]
                  (Position 10 7)
              )
              (Position 10 1)
          ]
      )
      `shouldBe` Left "Argument error: unexpected keyword argument z at 10:9"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 9 10)) (Position 9 3)] (Position 9 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 10 17)] (Position 10 13)) (Position 10 11)] (Position 10 7))
              (Position 10 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 10:13"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 11 10)) (Position 11 3)] (Position 11 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "b" (IdentifierExpr "missing" (Position 12 11)) (Position 12 11)] (Position 12 7))
              (Position 12 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 12:11"

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

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (IdentifierExpr "a" (Position 10 10)) (Position 10 3)] (Position 10 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 11 9),
                    IntegerExpr 2 (Position 11 12),
                    KeywordArgExpr "b" (IntegerExpr 3 (Position 11 17)) (Position 11 15),
                    KeywordArgExpr "a" (IntegerExpr 4 (Position 11 24)) (Position 11 22)
                  ]
                  (Position 11 7)
              )
              (Position 11 1)
          ]
      )
      `shouldBe` Left "Argument error: multiple values for parameter b at 11:15"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 10 10)) (Position 10 3)] (Position 10 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 11 9),
                    KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 11 20)] (Position 11 16)) (Position 11 14)
                  ]
                  (Position 11 7)
              )
              (Position 11 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 11:16"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 12 10)) (Position 12 3)] (Position 12 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 13 9),
                    KeywordArgExpr "a" (IdentifierExpr "missing" (Position 13 16)) (Position 13 14)
                  ]
                  (Position 13 7)
              )
              (Position 13 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 13:16"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 14 10)) (Position 14 3)] (Position 14 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 15 9),
                    KeywordArgExpr "a" (CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 15 24)] (Position 15 23)) (Position 15 21)] (Position 15 16)) (Position 15 14)
                  ]
                  (Position 15 7)
              )
              (Position 15 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 15:21"

    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 16 10)) (Position 16 3)] (Position 16 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 17 9),
                    KeywordArgExpr "a" (CallExpr "len" [IdentifierExpr "missing" (Position 17 20)] (Position 17 16)) (Position 17 14)
                  ]
                  (Position 17 7)
              )
              (Position 17 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 17:20"

  it "prioritizes first keyword evaluation error over duplicate keyword detection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 12 10)) (Position 12 3)] (Position 11 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 13 17)] (Position 13 13)) (Position 13 11),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 13 22)) (Position 13 20)
                  ]
                  (Position 13 7)
              )
              (Position 13 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 13:13"

  it "prioritizes duplicate keyword detection over second keyword evaluation error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 15 10)) (Position 15 3)] (Position 14 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 16 13)) (Position 16 11),
                    KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 16 24)] (Position 16 20)) (Position 16 18)
                  ]
                  (Position 16 7)
              )
              (Position 16 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 16:18"

  it "prioritizes first keyword Name error over duplicate keyword detection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 17 10)) (Position 17 3)] (Position 17 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IdentifierExpr "missing" (Position 18 11)) (Position 18 11),
                    KeywordArgExpr "a" (IntegerExpr 2 (Position 18 22)) (Position 18 20)
                  ]
                  (Position 18 7)
              )
              (Position 18 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 18:11"

  it "prioritizes duplicate keyword detection over second keyword Name error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a"] [ReturnStmt (IdentifierExpr "a" (Position 20 10)) (Position 20 3)] (Position 19 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 21 13)) (Position 21 11),
                    KeywordArgExpr "a" (IdentifierExpr "missing" (Position 21 20)) (Position 21 18)
                  ]
                  (Position 21 7)
              )
              (Position 21 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 21:18"

  it "reports first duplicate keyword argument in source order at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (IdentifierExpr "a" (Position 30 10)) (Position 30 3)] (Position 29 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 31 13)) (Position 31 11),
                    KeywordArgExpr "b" (IntegerExpr 2 (Position 31 20)) (Position 31 18),
                    KeywordArgExpr "a" (IntegerExpr 3 (Position 31 27)) (Position 31 25),
                    KeywordArgExpr "b" (IntegerExpr 4 (Position 31 34)) (Position 31 32)
                  ]
                  (Position 31 7)
              )
              (Position 31 1)
          ]
      )
      `shouldBe` Left "Argument error: duplicate keyword argument a at 31:25"

  it "prioritizes call-site binding errors over default builtin keyword rejection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [("c", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 18 24)] (Position 18 23)) (Position 18 21)] (Position 18 16))]
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
              [("c", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 22 24)] (Position 22 23)) (Position 22 21)] (Position 22 16))]
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
              [("c", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 26 24)] (Position 26 23)) (Position 26 21)] (Position 26 16))]
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

    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "len"
                  [KeywordArgExpr "x" (CallExpr "len" [IntegerExpr 1 (Position 16 17)] (Position 16 13)) (Position 16 11)]
                  (Position 16 7)
              )
              (Position 16 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 16:11"

    evalProgram
      ( Program
        [ PrintStmt
          ( CallExpr
            "len"
            [KeywordArgExpr "x" (IdentifierExpr "missing" (Position 17 13)) (Position 17 11)]
            (Position 17 7)
          )
          (Position 17 1)
        ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin len at 17:11"

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

    evalProgram
      ( Program
          [ PrintStmt
              ( CallExpr
                  "update"
                  [ DictExpr [] (Position 17 9),
                    KeywordArgExpr "k" (CallExpr "len" [IntegerExpr 1 (Position 17 27)] (Position 17 23)) (Position 17 20)
                  ]
                  (Position 17 7)
              )
              (Position 17 1)
          ]
      )
      `shouldBe` Left "Argument error: keyword arguments are not supported for builtin update at 17:20"

  it "reports runtime error position from default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", BinaryExpr DivideOperator (IntegerExpr 1 (Position 18 14)) (IntegerExpr 0 (Position 18 18)) (Position 18 16))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 19 10)) (IdentifierExpr "b" (Position 19 14)) (Position 19 12)) (Position 19 3)]
              (Position 18 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 20 9)] (Position 20 7)) (Position 20 1)
          ]
      )
      `shouldBe` Left "Value error: division by zero at 18:16"

  it "reports Name error position from default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", IdentifierExpr "missing" (Position 22 14))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 23 10)) (IdentifierExpr "b" (Position 23 14)) (Position 23 12)) (Position 23 3)]
              (Position 22 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 24 9)] (Position 24 7)) (Position 24 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 22:14"

  it "reports builtin type error position from default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", CallExpr "len" [IntegerExpr 1 (Position 26 18)] (Position 26 14))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 27 10)) (IdentifierExpr "b" (Position 27 14)) (Position 27 12)) (Position 27 3)]
              (Position 26 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 28 9)] (Position 28 7)) (Position 28 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 26:14"

  it "reports Name error when default expression references unknown identifier at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [ ("b", IntegerExpr 2 (Position 30 14)),
                ("c", IdentifierExpr "d" (Position 30 21))
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 31 10)) (IdentifierExpr "b" (Position 31 14)) (Position 31 12))
                      (IdentifierExpr "c" (Position 31 18))
                      (Position 31 16)
                  )
                  (Position 31 3)
              ]
              (Position 30 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 32 9)] (Position 32 7)) (Position 32 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier d at 30:21"

  it "reports Name error when default expression references later parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [ ("b", IdentifierExpr "c" (Position 34 14)),
                ("c", IntegerExpr 2 (Position 34 21))
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 35 10)) (IdentifierExpr "b" (Position 35 14)) (Position 35 12))
                      (IdentifierExpr "c" (Position 35 18))
                      (Position 35 16)
                  )
                  (Position 35 3)
              ]
              (Position 34 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 36 9)] (Position 36 7)) (Position 36 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier c at 34:14"

  it "reports Name error position from composite default expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [ ("b", IntegerExpr 2 (Position 38 14)),
                ("c", BinaryExpr AddOperator (IdentifierExpr "b" (Position 38 21)) (IdentifierExpr "d" (Position 38 25)) (Position 38 23))
              ]
              [ ReturnStmt
                  ( BinaryExpr
                      AddOperator
                      (BinaryExpr AddOperator (IdentifierExpr "a" (Position 39 10)) (IdentifierExpr "b" (Position 39 14)) (Position 39 12))
                      (IdentifierExpr "c" (Position 39 18))
                      (Position 39 16)
                  )
                  (Position 39 3)
              ]
              (Position 38 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 40 9)] (Position 40 7)) (Position 40 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier d at 38:25"

  it "reports builtin type error from default expression using bound parameter at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", CallExpr "len" [IdentifierExpr "a" (Position 42 18)] (Position 42 14))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 43 10)) (IdentifierExpr "b" (Position 43 14)) (Position 43 12)) (Position 43 3)]
              (Position 42 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 44 9)] (Position 44 7)) (Position 44 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 42:14"

  it "prioritizes explicit argument evaluation error over default expression error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", BinaryExpr DivideOperator (IntegerExpr 1 (Position 46 14)) (IntegerExpr 0 (Position 46 18)) (Position 46 16))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 47 10)) (IdentifierExpr "b" (Position 47 14)) (Position 47 12)) (Position 47 3)]
              (Position 46 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 48 17)] (Position 48 13)) (Position 48 11)] (Position 48 7))
              (Position 48 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 48:13"

  it "prioritizes positional argument evaluation error over default expression error at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", BinaryExpr DivideOperator (IntegerExpr 1 (Position 50 14)) (IntegerExpr 0 (Position 50 18)) (Position 50 16))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 51 10)) (IdentifierExpr "b" (Position 51 14)) (Position 51 12)) (Position 51 3)]
              (Position 50 1),
            PrintStmt
              (CallExpr "f" [CallExpr "len" [IntegerExpr 1 (Position 52 15)] (Position 52 11)] (Position 52 7))
              (Position 52 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 52:11"

  it "prioritizes explicit argument evaluation error over default builtin keyword rejection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 54 22)] (Position 54 21)) (Position 54 20)] (Position 54 14))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 55 10)) (IdentifierExpr "b" (Position 55 14)) (Position 55 12)) (Position 55 3)]
              (Position 54 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 56 17)] (Position 56 13)) (Position 56 11)] (Position 56 7))
              (Position 56 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 56:13"

  it "prioritizes positional argument evaluation error over default builtin keyword rejection at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 58 22)] (Position 58 21)) (Position 58 20)] (Position 58 14))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 59 10)) (IdentifierExpr "b" (Position 59 14)) (Position 59 12)) (Position 59 3)]
              (Position 58 1),
            PrintStmt
              (CallExpr "f" [CallExpr "len" [IntegerExpr 1 (Position 60 15)] (Position 60 11)] (Position 60 7))
              (Position 60 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 60:11"

  it "prioritizes count mismatch over default builtin keyword rejection with keyword call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [("c", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 62 24)] (Position 62 23)) (Position 62 21)] (Position 62 16))]
              [ReturnStmt (IdentifierExpr "a" (Position 63 10)) (Position 63 3)]
              (Position 62 1),
            PrintStmt
              (CallExpr "f" [KeywordArgExpr "a" (IntegerExpr 1 (Position 64 11)) (Position 64 9)] (Position 64 7))
              (Position 64 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling f at 64:7"

  it "prioritizes count mismatch over default builtin keyword rejection with positional call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b", "c"]
              [("c", CallExpr "len" [KeywordArgExpr "x" (ListExpr [IntegerExpr 1 (Position 66 24)] (Position 66 23)) (Position 66 21)] (Position 66 16))]
              [ReturnStmt (IdentifierExpr "a" (Position 67 10)) (Position 67 3)]
              (Position 66 1),
            PrintStmt
              (CallExpr "f" [IntegerExpr 1 (Position 68 9)] (Position 68 7))
              (Position 68 1)
          ]
      )
      `shouldBe` Left "Argument count mismatch when calling f at 68:7"

  it "prioritizes leftmost argument evaluation error in mixed positional and keyword call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 54 10)) (IdentifierExpr "b" (Position 54 14)) (Position 54 12)) (Position 54 3)] (Position 53 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ CallExpr "len" [IntegerExpr 1 (Position 55 13)] (Position 55 9),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 55 24)] (Position 55 20)) (Position 55 18)
                  ]
                  (Position 55 7)
              )
              (Position 55 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 55:9"

  it "prioritizes leftmost argument evaluation error in keyword-only call at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 57 10)) (IdentifierExpr "b" (Position 57 14)) (Position 57 12)) (Position 57 3)] (Position 56 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (CallExpr "len" [IntegerExpr 1 (Position 58 17)] (Position 58 13)) (Position 58 11),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 58 27)] (Position 58 23)) (Position 58 21)
                  ]
                  (Position 58 7)
              )
              (Position 58 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 58:13"

  it "reports right keyword argument evaluation error after left keyword succeeds at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 60 10)) (IdentifierExpr "b" (Position 60 14)) (Position 60 12)) (Position 60 3)] (Position 59 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ KeywordArgExpr "a" (IntegerExpr 1 (Position 61 13)) (Position 61 11),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 61 23)] (Position 61 19)) (Position 61 17)
                  ]
                  (Position 61 7)
              )
              (Position 61 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 61:19"

  it "reports keyword argument evaluation error after positional argument succeeds at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefStmt "f" ["a", "b"] [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 63 10)) (IdentifierExpr "b" (Position 63 14)) (Position 63 12)) (Position 63 3)] (Position 62 1),
            PrintStmt
              ( CallExpr
                  "f"
                  [ IntegerExpr 1 (Position 64 9),
                    KeywordArgExpr "b" (CallExpr "len" [IntegerExpr 1 (Position 64 19)] (Position 64 15)) (Position 64 13)
                  ]
                  (Position 64 7)
              )
              (Position 64 1)
          ]
      )
      `shouldBe` Left "Type error: len expects string or list at 64:15"

  it "reports Name error position inside builtin default list expression at evaluator layer" $ do
    evalProgram
      ( Program
          [ FunctionDefDefaultsStmt
              "f"
              ["a", "b"]
              [("b", CallExpr "len" [ListExpr [IdentifierExpr "a" (Position 46 19), IdentifierExpr "missing" (Position 46 22)] (Position 46 18)] (Position 46 14))]
              [ReturnStmt (BinaryExpr AddOperator (IdentifierExpr "a" (Position 47 10)) (IdentifierExpr "b" (Position 47 14)) (Position 47 12)) (Position 47 3)]
              (Position 46 1),
            PrintStmt (CallExpr "f" [IntegerExpr 1 (Position 48 9)] (Position 48 7)) (Position 48 1)
          ]
      )
      `shouldBe` Left "Name error: undefined identifier missing at 46:22"

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
          [ PrintStmt (CallExpr "get" [DictExpr [(IntegerExpr 1 (Position 24 13), IntegerExpr 2 (Position 24 16))] (Position 24 12), IntegerExpr 3 (Position 24 20)] (Position 24 7)) (Position 24 1)
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
