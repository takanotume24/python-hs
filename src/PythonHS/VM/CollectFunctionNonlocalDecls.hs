module PythonHS.VM.CollectFunctionNonlocalDecls (collectFunctionNonlocalDecls) where

import Data.Map.Strict qualified as Map
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (DeclareNonlocal), declareNonlocalName, declareNonlocalPos)

collectFunctionNonlocalDecls :: [Instruction] -> Map.Map String Position
collectFunctionNonlocalDecls functionCode =
  foldl collect Map.empty functionCode
  where
    collect acc instruction =
      case instruction of
        DeclareNonlocal {declareNonlocalName = name, declareNonlocalPos = pos} ->
          Map.insert name pos acc
        _ -> acc
