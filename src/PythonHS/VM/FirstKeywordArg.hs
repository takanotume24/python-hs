module PythonHS.VM.FirstKeywordArg (firstKeywordArg) where

import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.KeywordArgInfo (KeywordArgInfo (..))

firstKeywordArg :: [CompiledCallArg Instruction] -> Maybe KeywordArgInfo
firstKeywordArg compiledArgs =
  case compiledArgs of
    [] -> Nothing
    arg : restCompiledArgs ->
      case compiledCallArgKeyword arg of
        Nothing -> firstKeywordArg restCompiledArgs
        Just argName ->
          if argName == starArgMarker
            then firstKeywordArg restCompiledArgs
            else Just KeywordArgInfo {keywordArgInfoName = argName, keywordArgInfoPos = compiledCallArgPos arg}
  where
    starArgMarker = "__python_hs_star_arg__"
