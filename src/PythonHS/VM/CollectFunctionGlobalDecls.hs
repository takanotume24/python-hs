module PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls) where

import qualified Data.Set as Set
import PythonHS.VM.Instruction (Instruction (DeclareGlobal))

collectFunctionGlobalDecls :: [Instruction] -> Set.Set String
collectFunctionGlobalDecls functionCode =
  foldl collect Set.empty functionCode
  where
    collect acc instruction =
      case instruction of
        DeclareGlobal name -> Set.insert name acc
        _ -> acc
