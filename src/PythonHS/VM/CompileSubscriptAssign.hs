module PythonHS.VM.CompileSubscriptAssign (compileSubscriptAssign) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileSubscriptAssignConfig (CompileSubscriptAssignConfig (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.Instruction (Instruction (..))

compileSubscriptAssign :: CompileSubscriptAssignConfig -> Either String CompileExprResult
compileSubscriptAssign config =
  case compileSubscriptAssignTarget config of
    IndexExpr {indexExprBase = baseExpr, indexExprIndex = indexExpr} ->
      case baseName baseExpr of
        Just rootName -> do
          baseResult <- compileExpr 0 baseExpr
          indexResult <- compileExpr 0 indexExpr
          valueResult <- compileExpr 0 valueExpr
          let callArgs = [toArg baseResult, toArg indexResult, toArg valueResult]
              code =
                [ CallFunction
                    { callFunctionName = "__python_hs_setitem__",
                      callFunctionArgs = callArgs,
                      callFunctionPos = pos
                    },
                  StoreName {storeNameName = rootName}
                ]
          pure (CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = baseIndex + 2})
        Nothing -> Left ("VM compile error: unsupported subscript assignment target at " ++ showPos pos)
    _ -> Left ("VM compile error: unsupported subscript assignment target at " ++ showPos pos)
  where
    compileExpr = compileSubscriptAssignCompileExpr config
    baseIndex = compileSubscriptAssignBaseIndex config
    valueExpr = compileSubscriptAssignValue config
    pos = compileSubscriptAssignPos config

    baseName expr =
      case expr of
        IdentifierExpr {identifierExprName = name} -> Just name
        _ -> Nothing

    toArg result =
      CompiledCallArg
        { compiledCallArgCode = compileExprResultCode result,
          compiledCallArgKeyword = Nothing,
          compiledCallArgPos = pos
        }
