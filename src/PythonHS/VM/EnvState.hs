module PythonHS.VM.EnvState (EnvState (..)) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.Instruction (Instruction)

data EnvState = EnvState
  { envGlobals :: Map.Map String Value,
    envLocals :: Map.Map String Value,
    envFunctions :: Map.Map String ([String], [(String, [Instruction])], [Instruction]),
    envGlobalDecls :: Set.Set String
  }
  deriving (Show)
