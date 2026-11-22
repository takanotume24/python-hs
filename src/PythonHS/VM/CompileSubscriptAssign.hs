module PythonHS.VM.CompileSubscriptAssign (compileSubscriptAssign) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompileSubscriptAssignConfig (CompileSubscriptAssignConfig (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.Instruction (Instruction (..))

compileSubscriptAssign :: CompileSubscriptAssignConfig -> Either String CompileExprResult
compileSubscriptAssign config =
  case target of
    IndexExpr {indexExprBase = baseExpr, indexExprIndex = indexExpr} -> do
      rootName <- baseName baseExpr
      baseResult <- compileExpr 0 baseExpr
      indexResult <- compileExpr 0 indexExpr
      valueResult <- compileExpr 0 valueExpr
      pure (storeResult rootName "__python_hs_setitem__" [baseResult, indexResult, valueResult])
    SliceExpr {sliceExprBase = baseExpr, sliceExprStart = maybeStart, sliceExprEnd = maybeEnd} -> do
      rootName <- baseName baseExpr
      baseResult <- compileExpr 0 baseExpr
      startResult <- compileOptional maybeStart
      endResult <- compileOptional maybeEnd
      valueResult <- compileExpr 0 valueExpr
      pure (storeResult rootName "__python_hs_setslice__" [baseResult, startResult, endResult, valueResult])
    _ -> Left ("VM compile error: unsupported subscript assignment target at " ++ showPos pos)
  where
    compileExpr = compileSubscriptAssignCompileExpr config
    baseIndex = compileSubscriptAssignBaseIndex config
    target = compileSubscriptAssignTarget config
    valueExpr = compileSubscriptAssignValue config
    pos = compileSubscriptAssignPos config

    baseName expr =
      case expr of
        IdentifierExpr {identifierExprName = name} -> Right name
        _ -> Left ("VM compile error: unsupported subscript assignment target at " ++ showPos pos)

    compileOptional maybeExpr =
      case maybeExpr of
        Nothing -> Right (CompileExprResult {compileExprResultCode = [PushConst {pushConstValue = NoneValue}], compileExprResultEndIndex = baseIndex + 1})
        Just expr -> compileExpr 0 expr

    storeResult rootName functionName argResults =
      let callArgs = fmap toArg argResults
          code =
            [ CallFunction
                { callFunctionName = functionName,
                  callFunctionArgs = callArgs,
                  callFunctionPos = pos
                },
              StoreName {storeNameName = rootName}
            ]
       in CompileExprResult {compileExprResultCode = code, compileExprResultEndIndex = baseIndex + 2}

    toArg result =
      CompiledCallArg
        { compiledCallArgCode = compileExprResultCode result,
          compiledCallArgKeyword = Nothing,
          compiledCallArgPos = pos
        }
