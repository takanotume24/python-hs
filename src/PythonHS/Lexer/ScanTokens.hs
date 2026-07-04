module PythonHS.Lexer.ScanTokens (scanTokens) where

import Data.Char (isSpace)
import PythonHS.Lexer.AdjustIndentResult (AdjustIndentResult (..))
import PythonHS.Lexer.DedentToResult (DedentToResult (..))
import PythonHS.Lexer.LexerError (LexerError (..))
import PythonHS.Lexer.Position (Position (..), column)
import PythonHS.Lexer.ScanTokenStep (scanTokenStep)
import PythonHS.Lexer.ScanTokenStepConfig (ScanTokenStepConfig (..))
import PythonHS.Lexer.ScanTokenStepResult (ScanTokenStepResult (..))
import PythonHS.Lexer.Token (Token (..))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( DedentToken,
        EOFToken,
        IndentToken,
        NewlineToken
      ),
  )

scanTokens :: String -> Either LexerError [Token]
scanTokens input = go input 1 1 True [0] []
  where
    -- go input currentLine currentColumn isLineStart indentStack acc(reversed)
    go :: String -> Int -> Int -> Bool -> [Int] -> [Token] -> Either LexerError [Token]
    go [] ln col _ indentStack acc =
      let dedents = map (\_ -> Token {tokenType = DedentToken, lexeme = "<DEDENT>", position = Position {line = ln, column = 1}}) (drop 1 indentStack)
          finalTokens = dedents ++ [Token {tokenType = EOFToken, lexeme = "", position = Position {line = ln, column = col}}]
       in Right (reverse (reverse finalTokens ++ acc))
    go src ln col True indentStack acc =
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
              let tok = Token {tokenType = NewlineToken, lexeme = "\\n", position = Position {line = ln, column = col + indent}}
               in go rest' (ln + 1) 1 True indentStack (tok : acc)
            _ -> do
              AdjustIndentResult {adjustIndentResultStack = newStack, adjustIndentResultTokens = indentTokens} <- adjustIndent ln indentStack indent
              go restAfterSpaces ln (1 + indent) False newStack (reverse indentTokens ++ acc)
    go ('\n' : rest) ln col _ indentStack acc =
      let tok = Token {tokenType = NewlineToken, lexeme = "\\n", position = Position {line = ln, column = col}}
       in go rest (ln + 1) 1 True indentStack (tok : acc)
    go src ln col False indentStack acc =
      case src of
        (c : rest)
          | isSpace c -> go rest ln (col + 1) False indentStack acc
          | otherwise -> do
              result <- scanTokenStep (ScanTokenStepConfig {scanTokenStepSource = src, scanTokenStepPosition = Position {line = ln, column = col}})
              let tok = scanTokenStepResultToken result
                  restAfterToken = scanTokenStepResultRemaining result
                  nextCol = column (scanTokenStepResultNextPosition result)
              go restAfterToken ln nextCol False indentStack (tok : acc)

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
