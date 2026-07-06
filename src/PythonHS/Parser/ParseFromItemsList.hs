module PythonHS.Parser.ParseFromItemsList (parseFromItemsList) where

import PythonHS.AST.FromImportEntry (FromImportEntry (..))
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseFromItem (parseFromItem)
import PythonHS.Parser.ParseFromItemsTail (parseFromItemsTail)
import PythonHS.Parser.ParseFromItemsTailConfig (ParseFromItemsTailConfig (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseFromItemsList :: [Token] -> Either ParseError (ParseResult [FromImportEntry])
parseFromItemsList ts = do
  firstItemResult <- parseFromItem ts
  let firstItem = parseResultValue firstItemResult
      afterFirst = parseResultRemaining firstItemResult
  parseFromItemsTail (ParseFromItemsTailConfig {parseFromItemsTailItems = [firstItem], parseFromItemsTailTokenStream = afterFirst})
