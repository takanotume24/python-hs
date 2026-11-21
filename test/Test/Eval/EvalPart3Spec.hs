{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Eval.EvalPart3Spec (spec) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.AST.Program (Program (Program))
import PythonHS.AST.Stmt (Stmt (AssignStmt, BreakStmt, ContinueStmt, ForStmt, FunctionDefStmt, GlobalStmt, IfStmt, PassStmt, PrintStmt, ReturnStmt, WhileStmt))
import PythonHS.Evaluator (evalProgram)
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "evalProgram - part 3" $ do
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
              (DictExpr [DictEntry {dictEntryKey = IntegerExpr 3 (Position 0 0), dictEntryValue = IntegerExpr 30 (Position 0 0)}, DictEntry {dictEntryKey = IntegerExpr 1 (Position 0 0), dictEntryValue = IntegerExpr 10 (Position 0 0)}] (Position 0 0))
              [PrintStmt (IdentifierExpr "k" (Position 0 0)) (Position 0 0)]
              (Position 0 0)
          ]
      )
      `shouldBe` Right ["3", "1"]
