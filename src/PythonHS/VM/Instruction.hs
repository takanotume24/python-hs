module PythonHS.VM.Instruction (Instruction (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data Instruction
  = PushConst {pushConstValue :: Value}
  | LoadName
      { loadNameName :: String,
        loadNamePos :: Position
      }
  | StoreName {storeNameName :: String}
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
        defineFunctionDefaultCodes :: [(String, [Instruction])],
        defineFunctionCode :: [Instruction]
      }
  | CreateLambda
      { createLambdaName :: String,
        createLambdaParams :: [String],
        createLambdaDefaultCodes :: [(String, [Instruction])],
        createLambdaCode :: [Instruction]
      }
  | DefineClass
      { defineClassName :: String,
        defineClassBase :: Maybe String,
        defineClassMethods :: [(String, String)]
      }
  | BuildListComprehension
      { buildListComprehensionClauses :: [([String], [Instruction], [[Instruction]])],
        buildListComprehensionValueCode :: [Instruction],
        buildListComprehensionPos :: Position
      }
  | CallFunction
      { callFunctionName :: String,
        callFunctionArgs :: [([Instruction], Maybe String, Position)],
        callFunctionPos :: Position
      }
  | CallValueFunction
      { callValueFunctionArgs :: [([Instruction], Maybe String, Position)],
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
  | CheckWithResult
  | Halt
  deriving (Eq, Show)
