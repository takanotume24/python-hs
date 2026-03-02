module PythonHS.VM.CompileClassStmt (compileClassStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (FunctionDefDefaultsStmt, FunctionDefStmt), Stmt)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.VM.Instruction (Instruction (DefineClass, DefineFunction, LoadName, PushConst, ReturnTop), Instruction)

compileClassStmt ::
  ((Int -> Expr -> Either String ([Instruction], Int)) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int)) ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String ([Instruction], Int)) ->
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  Int ->
  String ->
  Maybe String ->
  [Stmt] ->
  Either String ([Instruction], Int)
compileClassStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body =
  let methods = collectMethods body
      compileMethodAt idx (methodName, params, defaults, methodBody, methodPos) = do
        let mangledName = className ++ "." ++ methodName
        (defaultCodes, _) <- compileDefaults compileExprAt defaults
        (bodyCode, _) <- compileStatements 0 True Nothing methodBody
        let functionCode =
              if methodName == "__init__"
                then bodyCode ++ [LoadName "self" methodPos, ReturnTop]
                else bodyCode ++ [PushConst (IntValue 0), ReturnTop]
        let methodInstr = DefineFunction mangledName params defaultCodes functionCode
        Right ([methodInstr], idx + 1, (methodName, mangledName))
      compileMethodsAt idx [] = Right ([], idx, [])
      compileMethodsAt idx (method : restMethods) = do
        (methodCode, afterMethod, methodPair) <- compileMethodAt idx method
        (restCode, afterRest, restPairs) <- compileMethodsAt afterMethod restMethods
        Right (methodCode ++ restCode, afterRest, methodPair : restPairs)
   in do
        (methodCode, afterMethods, methodPairs) <- compileMethodsAt baseIndex methods
        let classCode = methodCode ++ [DefineClass className maybeBase methodPairs]
        Right (classCode, afterMethods + 1)
  where
    collectMethods items =
      case items of
        [] -> []
        item : restItems ->
          case item of
            FunctionDefStmt methodName params methodBody methodPos ->
              (methodName, params, [], methodBody, methodPos) : collectMethods restItems
            FunctionDefDefaultsStmt methodName params defaults methodBody methodPos ->
              (methodName, params, defaults, methodBody, methodPos) : collectMethods restItems
            _ -> collectMethods restItems
