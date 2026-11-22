module PythonHS.Parser.ParseDictDisplay (parseDictDisplay) where

import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.Token (Token (Token), position)
import PythonHS.Lexer.TokenType (TokenType (ColonToken, CommaToken, DoubleStarToken, ForToken, RBraceToken))
import PythonHS.Parser.DesugarStarredDict (desugarStarredDict)
import PythonHS.Parser.ParseDictDisplayConfig (ParseDictDisplayConfig (..))
import PythonHS.Parser.ParseError (ParseError (..))
import PythonHS.Parser.ParseResult (ParseResult (..))

parseDictDisplay :: ParseDictDisplayConfig -> Either ParseError (ParseResult Expr)
parseDictDisplay config =
  case parseDictDisplayTokenStream config of
    Token RBraceToken _ _ : rest ->
      Right (ParseResult {parseResultValue = DictExpr {dictExprEntries = [], dictExprPos = parseDictDisplayPos config}, parseResultRemaining = rest})
    Token DoubleStarToken _ starPos : rest -> do
      valueResult <- parseDictDisplayExpr config rest
      let valueExpr = parseResultValue valueResult
          afterValue = parseResultRemaining valueResult
      parseDictTail [spreadEntry starPos valueExpr] afterValue
    ts -> do
      keyResult <- parseDictDisplayExpr config ts
      let keyExpr = parseResultValue keyResult
          afterKey = parseResultRemaining keyResult
      case afterKey of
        Token ColonToken _ _ : afterColon -> do
          valueResult <- parseDictDisplayExpr config afterColon
          let valueExpr = parseResultValue valueResult
              afterValue = parseResultRemaining valueResult
          parseDictTail [DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr}] afterValue
        forTokens@(Token ForToken _ _ : _) -> do
          comprehensionResult <- parseDictDisplayComprehension config keyExpr forTokens
          let comprehensionExpr = parseResultValue comprehensionResult
          Right (ParseResult {parseResultValue = setExprFromList comprehensionExpr, parseResultRemaining = parseResultRemaining comprehensionResult})
        commaTokens@(Token CommaToken _ _ : _) -> parseSetTail [keyExpr] commaTokens
        Token RBraceToken _ _ : afterBrace ->
          Right (ParseResult {parseResultValue = setExprFromList (ListExpr {listExprItems = [keyExpr], listExprPos = parseDictDisplayPos config}), parseResultRemaining = afterBrace})
        tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
        _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
  where
    setExprFromList listExpr =
      CallExpr {callExprName = "set", callExprArgs = [listExpr], callExprPos = parseDictDisplayPos config}

    spreadEntry starPos valueExpr =
      DictEntry {dictEntryKey = KwStarArgExpr {kwStarArgExprValue = valueExpr, kwStarArgExprPos = starPos}, dictEntryValue = NoneExpr {noneExprPos = starPos}}

    parseSetTail items (Token RBraceToken _ _ : rest) =
      Right (ParseResult {parseResultValue = setExprFromList (ListExpr {listExprItems = items, listExprPos = parseDictDisplayPos config}), parseResultRemaining = rest})
    parseSetTail items (Token CommaToken _ _ : rest) =
      case rest of
        Token RBraceToken _ _ : afterBrace ->
          Right (ParseResult {parseResultValue = setExprFromList (ListExpr {listExprItems = items, listExprPos = parseDictDisplayPos config}), parseResultRemaining = afterBrace})
        _ -> do
          nextResult <- parseDictDisplayExpr config rest
          let nextExpr = parseResultValue nextResult
          parseSetTail (items ++ [nextExpr]) (parseResultRemaining nextResult)
    parseSetTail _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseSetTail _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})

    parseDictTail pairs (Token CommaToken _ _ : rest) =
      case rest of
        Token RBraceToken _ _ : afterBrace -> Right (ParseResult {parseResultValue = desugarStarredDict (parseDictDisplayPos config) pairs, parseResultRemaining = afterBrace})
        Token DoubleStarToken _ starPos : afterStar -> do
          valueResult <- parseDictDisplayExpr config afterStar
          let valueExpr = parseResultValue valueResult
              afterValue = parseResultRemaining valueResult
          parseDictTail (pairs ++ [spreadEntry starPos valueExpr]) afterValue
        _ -> do
          nextKeyResult <- parseDictDisplayExpr config rest
          let nextKey = parseResultValue nextKeyResult
              afterKey = parseResultRemaining nextKeyResult
          case afterKey of
            Token ColonToken _ _ : afterColon -> do
              nextValueResult <- parseDictDisplayExpr config afterColon
              let nextValue = parseResultValue nextValueResult
                  afterValue = parseResultRemaining nextValueResult
              parseDictTail (pairs ++ [DictEntry {dictEntryKey = nextKey, dictEntryValue = nextValue}]) afterValue
            tok : _ -> Left (ExpectedExpression {parseErrorPosition = position tok})
            _ -> Left (ExpectedExpression {parseErrorPosition = Position 0 0})
    parseDictTail pairs (Token RBraceToken _ _ : rest) =
      Right (ParseResult {parseResultValue = desugarStarredDict (parseDictDisplayPos config) pairs, parseResultRemaining = rest})
    parseDictTail _ (tok : _) = Left (ExpectedExpression {parseErrorPosition = position tok})
    parseDictTail _ _ = Left (ExpectedExpression {parseErrorPosition = Position 0 0})
