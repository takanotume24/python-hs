module PythonHS.VM.ToCallArg (toCallArg) where

import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.ToCallArgInput (ToCallArgInput (..))

toCallArg :: ToCallArgInput -> CompiledCallArg Instruction
toCallArg input =
  CompiledCallArg
    { compiledCallArgCode = toCallArgInputCode input,
      compiledCallArgKeyword = toCallArgInputKeyword input,
      compiledCallArgPos = toCallArgInputPos input
    }
