module PythonHS.VM.ValidateNonlocalDeclsConfig (ValidateNonlocalDeclsConfig (..)) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data ValidateNonlocalDeclsConfig = ValidateNonlocalDeclsConfig
  { validateNonlocalDeclsParams :: [String],
    validateNonlocalDeclsNames :: Map String Position,
    validateNonlocalDeclsGlobalNames :: Set String,
    validateNonlocalDeclsLocals :: Map String Value
  }
