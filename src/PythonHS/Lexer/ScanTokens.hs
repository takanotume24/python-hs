module PythonHS.Lexer.ScanTokens (scanTokens) where

import Data.Char (isSpace)
import PythonHS.Lexer.AdjustIndentResult (AdjustIndentResult (..))
import PythonHS.Lexer.DedentToResult (DedentToResult (..))
import PythonHS.Lexer.LexerError (LexerError (..))
import PythonHS.Lexer.Position (Position (..), column)
import PythonHS.Lexer.ScanRawTripleQuotedString (scanRawTripleQuotedString)
import PythonHS.Lexer.ScanTokenStep (scanTokenStep)
import PythonHS.Lexer.ScanTokenStepConfig (ScanTokenStepConfig (..))
import PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..))
import PythonHS.Lexer.ScanTripleQuotedString (scanTripleQuotedString)
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( DedentToken,
        EOFToken,
        IndentToken,
        LBraceToken,
        LBracketToken,
        LParenToken,
        NewlineToken,
        RBraceToken,
        RBracketToken,
        RParenToken,
        StringToken
      ),
  )
import PythonHS.Lexer.TripleQuotedStringResult (TripleQuotedStringResult (..))

scanTokens :: String -> Either LexerError [Token]
scanTokens input = go input 1 1 True [0] [] 0
  where
    -- go input currentLine currentColumn isLineStart indentStack acc(reversed) bracketDepth
    go :: String -> Int -> Int -> Bool -> [Int] -> [Token] -> Int -> Either LexerError [Token]
    go [] ln col _ indentStack acc _ =
      let dedents = map (\_ -> Token {tokenType = DedentToken, lexeme = "<DEDENT>", position = Position {line = ln, column = 1}}) (drop 1 indentStack)
          finalTokens = dedents ++ [Token {tokenType = EOFToken, lexeme = "", position = Position {line = ln, column = col}}]
       in Right (reverse (reverse finalTokens ++ acc))
    go src ln col True indentStack acc bracketDepth =
      let spanResult = span (== ' ') src
          spaces = fst spanResult
          restAfterSpaces = snd spanResult
          indent = length spaces
       in case restAfterSpaces of
            [] ->
              let dedents = map (\_ -> Token {tokenType = DedentToken, lexeme = "<DEDENT>", position = Position {line = ln, column = 1}}) (drop 1 indentStack)
                  finalTokens = dedents ++ [Token {tokenType = EOFToken, lexeme = "", position = Position {line = ln, column = col + indent}}]
               in Right (reverse (reverse finalTokens ++ acc))
            ('\n' : rest') ->
              -- A blank line is not a logical line, so it must not emit a
              -- statement-terminating newline token.
              go rest' (ln + 1) 1 True indentStack acc bracketDepth
            ('#' : restComment) ->
              let (comment, restAfterComment) = span (/= '\n') restComment
               in case restAfterComment of
                    ('\n' : rest') -> go rest' (ln + 1) 1 True indentStack acc bracketDepth
                    _ -> go restAfterComment ln (col + indent + 1 + length comment) True indentStack acc bracketDepth
            _
              | bracketDepth > 0 ->
                  -- Inside brackets a newline is implicit continuation, so the
                  -- indentation of the next physical line is not significant.
                  go restAfterSpaces ln (1 + indent) False indentStack acc bracketDepth
              | otherwise -> do
                  AdjustIndentResult {adjustIndentResultStack = newStack, adjustIndentResultTokens = indentTokens} <- adjustIndent ln indentStack indent
                  go restAfterSpaces ln (1 + indent) False newStack (reverse indentTokens ++ acc) bracketDepth
    go ('\n' : rest) ln col _ indentStack acc bracketDepth
      | bracketDepth > 0 = go rest (ln + 1) 1 True indentStack acc bracketDepth
      | otherwise =
          let tok = Token {tokenType = NewlineToken, lexeme = "\\n", position = Position {line = ln, column = col}}
           in go rest (ln + 1) 1 True indentStack (tok : acc) bracketDepth
    go src ln col False indentStack acc bracketDepth =
      case scanRawTripleQuotedString src of
        Just result -> emitTripleQuotedString result
        Nothing ->
          case scanTripleQuotedString src of
            Just result -> emitTripleQuotedString result
            Nothing -> goToken src ln col indentStack acc bracketDepth
      where
        emitTripleQuotedString result =
          let tok = Token {tokenType = StringToken, lexeme = tripleQuotedStringResultContent result, position = Position {line = ln, column = col}}
              newLine = ln + tripleQuotedStringResultLineCount result
              newColumn = tripleQuotedStringResultEndColumn result
           in go (tripleQuotedStringResultRemaining result) newLine newColumn False indentStack (tok : acc) bracketDepth

    goToken src ln col indentStack acc bracketDepth =
      case src of
        ('\\' : '\n' : rest) ->
          -- An explicit line continuation joins the next physical line.
          go rest (ln + 1) 1 False indentStack acc bracketDepth
        (c : rest)
          | c == '#' ->
              let (comment, restAfterComment) = span (/= '\n') rest
               in go restAfterComment ln (col + 1 + length comment) False indentStack acc bracketDepth
          | isSpace c -> go rest ln (col + 1) False indentStack acc bracketDepth
          | c == ';' -> goSemicolon rest ln col indentStack acc bracketDepth
          | otherwise -> do
              result <- scanTokenStep (ScanTokenStepConfig {scanTokenStepSource = src, scanTokenStepPosition = Position {line = ln, column = col}})
              let tok = scanTokenStepResultToken result
                  restAfterToken = scanTokenStepResultRemaining result
                  nextCol = column (scanTokenStepResultNextPosition result)
                  nextDepth = adjustBracketDepth bracketDepth tok
              go restAfterToken ln nextCol False indentStack (tok : acc) nextDepth
        [] -> Left (UnexpectedCharacter {unexpectedChar = ' '})

    goSemicolon rest ln col indentStack acc bracketDepth =
      let restAfterSpaces = dropWhile (== ' ') rest
          consumedSpaces = length rest - length restAfterSpaces
       in case restAfterSpaces of
            [] -> go [] ln (col + 1) False indentStack acc bracketDepth
            ('\n' : _) -> go rest ln (col + 1) False indentStack acc bracketDepth
            ('#' : _) -> go rest ln (col + 1) False indentStack acc bracketDepth
            _ ->
              let tok = Token {tokenType = NewlineToken, lexeme = ";", position = Position {line = ln, column = col}}
               in go restAfterSpaces ln (col + 1 + consumedSpaces) False indentStack (tok : acc) bracketDepth

    adjustBracketDepth depth tok =
      case tokenType tok of
        LParenToken -> depth + 1
        LBracketToken -> depth + 1
        LBraceToken -> depth + 1
        RParenToken -> max 0 (depth - 1)
        RBracketToken -> max 0 (depth - 1)
        RBraceToken -> max 0 (depth - 1)
        _ -> depth

    adjustIndent :: Int -> [Int] -> Int -> Either LexerError AdjustIndentResult
    adjustIndent ln indentStack indent =
      case indentStack of
        [] -> Right AdjustIndentResult {adjustIndentResultStack = [indent], adjustIndentResultTokens = []}
        current : _
          | indent == current -> Right AdjustIndentResult {adjustIndentResultStack = indentStack, adjustIndentResultTokens = []}
          | indent > current -> Right AdjustIndentResult {adjustIndentResultStack = indent : indentStack, adjustIndentResultTokens = [Token {tokenType = IndentToken, lexeme = "<INDENT>", position = Position {line = ln, column = 1}}]}
          | otherwise ->
              case dedentTo ln indent indentStack [] of
                Left err -> Left err
                Right result ->
                  Right
                    AdjustIndentResult
                      { adjustIndentResultStack = dedentToResultStack result,
                        adjustIndentResultTokens = dedentToResultTokens result
                      }

    dedentTo :: Int -> Int -> [Int] -> [Token] -> Either LexerError DedentToResult
    dedentTo _ _ [] _ = Left (UnexpectedCharacter {unexpectedChar = ' '})
    dedentTo ln target (current : restStack) emitted
      | target == current = Right DedentToResult {dedentToResultStack = current : restStack, dedentToResultTokens = reverse emitted}
      | target < current = dedentTo ln target restStack (Token {tokenType = DedentToken, lexeme = "<DEDENT>", position = Position {line = ln, column = 1}} : emitted)
      | otherwise = Left (UnexpectedCharacter {unexpectedChar = ' '})
