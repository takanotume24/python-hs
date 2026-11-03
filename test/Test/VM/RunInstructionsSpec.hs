module Test.VM.RunInstructionsSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction)
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionApplyResult (ExceptionApplyResult (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction)
import PythonHS.VM.HandleExceptionInstructionConfig (HandleExceptionInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.RunInstructions (runInstructions)
import PythonHS.VM.RunInstructionsTrace (runInstructionsTrace)
import PythonHS.VM.TraceRunResult (TraceRunResult (..))
import PythonHS.VM.VMState (VMState (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "runInstructions (pinned exception flows)" $ do
  it "runs typed except stream directly" $ do
    let stream =
          [ PushExceptionHandler {pushExceptionHandlerIp = 5},
            CallFunction {callFunctionName = "len", callFunctionArgs = [CompiledCallArg {compiledCallArgCode = [PushConst {pushConstValue = IntValue {intValue = 1}}], compiledCallArgKeyword = Nothing, compiledCallArgPos = Position {line = 2, column = 13}}], callFunctionPos = Position {line = 2, column = 9}},
            PrintTop,
            PopExceptionHandler,
            Jump {jumpTarget = 21},
            MatchExceptionType {matchExceptionTypeName = Just ["NameError"]},
            JumpIfFalse {jumpIfFalseTarget = 10},
            PushConst {pushConstValue = IntValue {intValue = 1}},
            PrintTop,
            Jump {jumpTarget = 21},
            MatchExceptionType {matchExceptionTypeName = Just ["TypeError"]},
            JumpIfFalse {jumpIfFalseTarget = 15},
            PushConst {pushConstValue = IntValue {intValue = 2}},
            PrintTop,
            Jump {jumpTarget = 21},
            MatchExceptionType {matchExceptionTypeName = Nothing},
            JumpIfFalse {jumpIfFalseTarget = 20},
            PushConst {pushConstValue = IntValue {intValue = 3}},
            PrintTop,
            Jump {jumpTarget = 21},
            RaisePendingException,
            Halt
          ]
    runInstructions stream `shouldBe` Right ["2"]

  let initVMStateForFixture =
        VMState
          { vmCode = [PopExceptionHandler],
            vmIp = 0,
            vmStack = [],
            vmEnv = EnvState {envGlobals = Map.empty, envLocals = Map.empty, envFunctions = Map.empty, envGlobalDecls = Set.empty},
            vmLoop = LoopState {loopForStates = Map.empty, loopCounts = Map.empty},
            vmException = ExceptionState {exceptionHandlers = [8], exceptionOutputs = []},
            vmIsTopLevel = True,
            vmOutputs = [],
            vmTraceSteps = [],
            vmTraceFailure = Nothing
          }

  it "applies PopExceptionHandler purely" $
    applyExceptionInstruction 1 PopExceptionHandler [] Map.empty [8]
      `shouldBe` Right ExceptionApplyResult {exceptionApplyResultError = Nothing, exceptionApplyResultIp = 2, exceptionApplyResultStack = [], exceptionApplyResultHandlers = []}

  it "push handles exception before raise in stream" $
    runInstructions
      [ PushExceptionHandler {pushExceptionHandlerIp = 8},
        PushConst {pushConstValue = StringValue {stringValue = "late"}},
        RaiseTop {raiseTopPos = Position {line = 6, column = 3}},
        Halt
      ]
      `shouldBe` Right []

  it "propagates pop from empty handler stack" $
    runInstructions [PopExceptionHandler] `shouldBe` Left "Runtime error: attempting to pop from empty exception handler stack"

  it "handles PopExceptionHandler in one direct step" $
    ( exceptionHandlers . vmException
        <$> handleExceptionInstruction
          HandleExceptionInstructionConfig
            { handleExceptionInstructionState = initVMStateForFixture,
              handleExceptionInstructionInstruction = PopExceptionHandler
            }
    )
      `shouldBe` Right []

  it "propagates top-level raise with empty handler stack" $
    runInstructions
      [ PushConst {pushConstValue = IntValue {intValue = 1}},
        PrintTop,
        PushConst {pushConstValue = StringValue {stringValue = "late"}},
        RaiseTop {raiseTopPos = Position {line = 6, column = 3}},
        Halt
      ]
      `shouldBe` Left "Runtime error: late at 6:3"

  it "propagates raise after popping the only handler" $
    runInstructions
      [ PushExceptionHandler {pushExceptionHandlerIp = 8},
        PopExceptionHandler,
        PushConst {pushConstValue = StringValue {stringValue = "late"}},
        RaiseTop {raiseTopPos = Position {line = 6, column = 3}},
        Halt
      ]
      `shouldBe` Left "Runtime error: late at 6:3"

  it "agrees with runInstructionsTrace on the elseRaise stream" $ do
    let elseRaiseStream =
          [ PushExceptionHandler {pushExceptionHandlerIp = 8},
            PushConst {pushConstValue = IntValue {intValue = 1}},
            PrintTop,
            PopExceptionHandler,
            Jump {jumpTarget = 5},
            PushConst {pushConstValue = StringValue {stringValue = "late"}},
            RaiseTop {raiseTopPos = Position {line = 6, column = 3}},
            Jump {jumpTarget = 14},
            MatchExceptionType {matchExceptionTypeName = Nothing},
            JumpIfFalse {jumpIfFalseTarget = 13},
            PushConst {pushConstValue = IntValue {intValue = 9}},
            PrintTop,
            Jump {jumpTarget = 14},
            RaisePendingException,
            Halt
          ]
    case runInstructions elseRaiseStream of
      Right outputs -> runInstructionsTrace elseRaiseStream `shouldBe` TraceRunResult {traceRunResultOutputs = outputs, traceRunResultFailure = Nothing, traceRunResultSteps = []}
      Left err -> runInstructionsTrace elseRaiseStream `shouldSatisfy` (\result -> traceRunResultFailure result == Just err)

  it "traces the pop-only handler stream the same way" $
    runInstructionsTrace
      [ PushExceptionHandler {pushExceptionHandlerIp = 8},
        PopExceptionHandler,
        PushConst {pushConstValue = StringValue {stringValue = "late"}},
        RaiseTop {raiseTopPos = Position {line = 6, column = 3}},
        Halt
      ]
      `shouldSatisfy` (\result -> traceRunResultFailure result == Just "Runtime error: late at 6:3")

  it "propagates raise raised after try/except closes its handler" $ do
    let elseRaiseStream =
          [ PushExceptionHandler {pushExceptionHandlerIp = 8},
            PushConst {pushConstValue = IntValue {intValue = 1}},
            PrintTop,
            PopExceptionHandler,
            Jump {jumpTarget = 5},
            PushConst {pushConstValue = StringValue {stringValue = "late"}},
            RaiseTop {raiseTopPos = Position {line = 6, column = 3}},
            Jump {jumpTarget = 14},
            MatchExceptionType {matchExceptionTypeName = Nothing},
            JumpIfFalse {jumpIfFalseTarget = 13},
            PushConst {pushConstValue = IntValue {intValue = 9}},
            PrintTop,
            Jump {jumpTarget = 14},
            RaisePendingException,
            Halt
          ]
    runInstructions elseRaiseStream `shouldBe` Left "Runtime error: late at 6:3"
