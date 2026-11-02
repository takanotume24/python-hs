module PythonHS.VM.Instruction (Instruction (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompiledCallArg (CompiledCallArg)
import PythonHS.VM.CompiledComprehensionClause (CompiledComprehensionClause)
import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode)
import PythonHS.VM.MethodNamePair (MethodNamePair)

data Instruction
  = PushConst {pushConstValue :: Value}
  | LoadName
      { loadNameName :: String,
        loadNamePos :: Position
      }
  | StoreName {storeNameName :: String}
  | DeleteName {deleteNameTarget :: String}
  | BuildList {buildListCount :: Int}
  | BuildTuple {buildTupleCount :: Int}
  | BuildDict {buildDictCount :: Int}
  | DeclareGlobal {declareGlobalName :: String}
  | LoopGuard {loopGuardPos :: Position}
  | ForSetup
      { forSetupNextIndex :: Int,
        forSetupPos :: Position
      }
  | ForNext
      { forNextName :: String,
        forNextLoopEndIndex :: Int,
        forNextPos :: Position
      }
  | PushExceptionHandler {pushExceptionHandlerIp :: Int}
  | PushFinallyHandler {pushFinallyHandlerIp :: Int}
  | PushWithHandler {pushWithHandlerIp :: Int}
  | PopExceptionHandler
  | LoadPendingException
  | MatchExceptionType {matchExceptionTypeName :: Maybe String}
  | DupTop
  | ApplyUnaryMinus {applyUnaryMinusPos :: Position}
  | ApplyNot {applyNotPos :: Position}
  | ApplyBinary
      { applyBinaryOp :: BinaryOperator,
        applyBinaryPos :: Position
      }
  | MatchPattern
      { matchPatternPattern :: Pattern,
        matchPatternPos :: Position
      }
  | JumpIfFalse {jumpIfFalseTarget :: Int}
  | Jump {jumpTarget :: Int}
  | DefineFunction
      { defineFunctionName :: String,
        defineFunctionParams :: [String],
        defineFunctionDefaultCodes :: [CompiledDefaultCode Instruction],
        defineFunctionCode :: [Instruction]
      }
  | CreateLambda
      { createLambdaName :: String,
        createLambdaParams :: [String],
        createLambdaDefaultCodes :: [CompiledDefaultCode Instruction],
        createLambdaCode :: [Instruction]
      }
  | DefineClass
      { defineClassName :: String,
        defineClassBase :: Maybe String,
        defineClassMethods :: [MethodNamePair]
      }
  | StoreClassAttr
      { storeClassAttrClassName :: String,
        storeClassAttrAttrName :: String
      }
  | BuildListComprehension
      { buildListComprehensionClauses :: [CompiledComprehensionClause Instruction],
        buildListComprehensionValueCode :: [Instruction],
        buildListComprehensionPos :: Position
      }
  | CallFunction
      { callFunctionName :: String,
        callFunctionArgs :: [CompiledCallArg Instruction],
        callFunctionPos :: Position
      }
  | CallValueFunction
      { callValueFunctionArgs :: [CompiledCallArg Instruction],
        callValueFunctionPos :: Position
      }
  | UnpackToNames
      { unpackToNamesNames :: [String],
        unpackToNamesPos :: Position
      }
  | RaiseTop {raiseTopPos :: Position}
  | RaisePendingException
  | RaisePendingError
  | ReturnTop
  | PrintTop
  | PopTop
  | AssertTop {assertTopPos :: Position}
  | CheckWithResult
  | Halt
  deriving (Eq, Show)
