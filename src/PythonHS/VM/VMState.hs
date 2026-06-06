module PythonHS.VM.VMState
  ( VMState (..)
  , EnvState (..)
  , LoopState (..)
  , ExceptionState (..)
  , initVMState
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.Instruction (Instruction)

data EnvState = EnvState
  { envGlobals    :: Map.Map String Value
  , envLocals     :: Map.Map String Value
  , envFunctions  :: Map.Map String ([String], [(String, [Instruction])], [Instruction])
  , envGlobalDecls :: Set.Set String
  } deriving (Show)

data LoopState = LoopState
  { loopForStates  :: Map.Map Int [Value]
  , loopCounts     :: Map.Map Int Int
  } deriving (Show)

data ExceptionState = ExceptionState
  { exceptionHandlers :: [Int]
  , exceptionOutputs  :: [String]
  } deriving (Show)

data VMState = VMState
  { vmCode        :: [Instruction]
  , vmIp          :: Int
  , vmStack       :: [Value]
  , vmEnv         :: EnvState
  , vmLoop        :: LoopState
  , vmException   :: ExceptionState
  , vmIsTopLevel  :: Bool
  } deriving (Show)

initVMState :: [Instruction] -> VMState
initVMState instructions =
  VMState
    { vmCode = instructions
    , vmIp = 0
    , vmStack = []
    , vmEnv = EnvState
      { envGlobals = Map.empty
      , envLocals = Map.empty
      , envFunctions = Map.empty
      , envGlobalDecls = Set.empty
      }
    , vmLoop = LoopState
      { loopForStates = Map.empty
      , loopCounts = Map.empty
      }
    , vmException = ExceptionState
      { exceptionHandlers = []
      , exceptionOutputs = []
      }
    , vmIsTopLevel = True
    }
