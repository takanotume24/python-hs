module PythonHS.VM.CollectClassMethods (collectClassMethods) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.VM.MethodInfo (MethodInfo (..))
import PythonHS.VM.MethodKind (MethodKind (..))

collectClassMethods :: [Stmt] -> [MethodInfo]
collectClassMethods items =
  case items of
    [] -> []
    item : restItems ->
      case item of
        FunctionDefStmt {functionDefStmtName = methodName, functionDefStmtParams = params, functionDefStmtBody = methodBody, functionDefStmtPos = methodPos} ->
          MethodInfo {methodInfoName = methodName, methodInfoParams = params, methodInfoDefaults = [], methodInfoBody = methodBody, methodInfoPos = methodPos, methodInfoKind = InstanceMethod} : collectClassMethods restItems
        FunctionDefDefaultsStmt {functionDefDefaultsStmtName = methodName, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = methodBody, functionDefDefaultsStmtPos = methodPos} ->
          MethodInfo {methodInfoName = methodName, methodInfoParams = params, methodInfoDefaults = defaults, methodInfoBody = methodBody, methodInfoPos = methodPos, methodInfoKind = InstanceMethod} : collectClassMethods restItems
        DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = targetStmt} ->
          case targetStmt of
            FunctionDefStmt {functionDefStmtName = methodName, functionDefStmtParams = params, functionDefStmtBody = methodBody, functionDefStmtPos = methodPos} ->
              MethodInfo {methodInfoName = methodName, methodInfoParams = params, methodInfoDefaults = [], methodInfoBody = methodBody, methodInfoPos = methodPos, methodInfoKind = methodKindFromDecorators decorators} : collectClassMethods restItems
            FunctionDefDefaultsStmt {functionDefDefaultsStmtName = methodName, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = methodBody, functionDefDefaultsStmtPos = methodPos} ->
              MethodInfo {methodInfoName = methodName, methodInfoParams = params, methodInfoDefaults = defaults, methodInfoBody = methodBody, methodInfoPos = methodPos, methodInfoKind = methodKindFromDecorators decorators} : collectClassMethods restItems
            _ -> collectClassMethods restItems
        _ -> collectClassMethods restItems
  where
    methodKindFromDecorators decorators =
      case decorators of
        [IdentifierExpr {identifierExprName = "staticmethod"}] -> StaticMethod
        [IdentifierExpr {identifierExprName = "classmethod"}] -> ClassMethod
        _ -> InstanceMethod
