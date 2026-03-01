module PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths) where

import PythonHS.AST.Program (Program)
import PythonHS.Lexer.ScanTokens (scanTokens)
import PythonHS.Parser.ParseProgram (parseProgram)
import PythonHS.VM.CompileProgram (compileProgram)
import PythonHS.VM.ResolveLocalImports (resolveLocalImports)
import PythonHS.VM.RunInstructions (runInstructions)

runSourceVmWithSearchPaths :: [FilePath] -> String -> IO (Either String [String])
runSourceVmWithSearchPaths searchPaths src =
  case parseOnly src of
    Left err -> pure (Left err)
    Right program -> do
      resolved <- resolveLocalImports searchPaths program
      case resolved of
        Left err -> pure (Left err)
        Right resolvedProgram ->
          case compileProgram resolvedProgram of
            Left compileErr -> pure (Left compileErr)
            Right instructions -> pure (runInstructions instructions)
  where
    parseOnly source = do
      tokens <- either (Left . show) Right (scanTokens source)
      parsed <- either (Left . show) Right (parseProgram tokens)
      Right (parsed :: Program)
