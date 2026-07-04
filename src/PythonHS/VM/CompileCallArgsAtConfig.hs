module PythonHS.VM.CompileCallArgsAtConfig (CompileCallArgsAtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)

data CompileCallArgsAtConfig = CompileCallArgsAtConfig
  { compileCallArgsAtCompileExpr :: Int -> Expr -> Either String ([Instruction], Int),
    compileCallArgsAtArgs :: [Expr]
  }
