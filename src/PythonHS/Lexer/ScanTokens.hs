module PythonHS.Lexer.ScanTokens (scanTokens) where

import Data.Char (isSpace)
import PythonHS.Lexer.LexerError (LexerError (UnexpectedCharacter))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.ScanTokenStep (scanTokenStep)
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( EOFToken,
        NewlineToken,
        IndentToken,
        DedentToken
      )
  )

scanTokens :: String -> Either LexerError [Token]
scanTokens input = go input 1 1 True [0] []
  where
    -- go input currentLine currentColumn isLineStart indentStack acc(reversed)
    go :: String -> Int -> Int -> Bool -> [Int] -> [Token] -> Either LexerError [Token]
    go [] ln col _ indentStack acc =
      let dedents = map (\_ -> Token DedentToken "<DEDENT>" (Position ln 1)) (drop 1 indentStack)
          finalTokens = dedents ++ [Token EOFToken "" (Position ln col)]
       in Right (reverse (reverse finalTokens ++ acc))

    go src ln col True indentStack acc =
      let (spaces, restAfterSpaces) = span (== ' ') src
          indent = length spaces
       in case restAfterSpaces of
            [] ->
              let dedents = map (\_ -> Token DedentToken "<DEDENT>" (Position ln 1)) (drop 1 indentStack)
                  finalTokens = dedents ++ [Token EOFToken "" (Position ln (col + indent))]
               in Right (reverse (reverse finalTokens ++ acc))
            ('\n' : rest') ->
              let tok = Token NewlineToken "\\n" (Position ln (col + indent))
               in go rest' (ln + 1) 1 True indentStack (tok : acc)
            _ -> do
              (newStack, indentTokens) <- adjustIndent ln indentStack indent
              go restAfterSpaces ln (1 + indent) False newStack (reverse indentTokens ++ acc)

    go ('\n' : rest) ln col _ indentStack acc =
      let tok = Token NewlineToken "\\n" (Position ln col)
       in go rest (ln + 1) 1 True indentStack (tok : acc)

    go src ln col False indentStack acc =
      case src of
        (c : rest)
          | isSpace c -> go rest ln (col + 1) False indentStack acc
          | otherwise -> do
              (tok, restAfterToken, nextCol) <- scanTokenStep src ln col
              go restAfterToken ln nextCol False indentStack (tok : acc)

    adjustIndent :: Int -> [Int] -> Int -> Either LexerError ([Int], [Token])
    adjustIndent ln indentStack indent =
      case indentStack of
        [] -> Right ([indent], [])
        current : _
          | indent == current -> Right (indentStack, [])
          | indent > current -> Right (indent : indentStack, [Token IndentToken "<INDENT>" (Position ln 1)])
          | otherwise -> dedentTo ln indent indentStack []

    dedentTo :: Int -> Int -> [Int] -> [Token] -> Either LexerError ([Int], [Token])
    dedentTo _ _ [] _ = Left (UnexpectedCharacter ' ')
    dedentTo ln target (current : restStack) emitted
      | target == current = Right (current : restStack, reverse emitted)
      | target < current = dedentTo ln target restStack (Token DedentToken "<DEDENT>" (Position ln 1) : emitted)
      | otherwise = Left (UnexpectedCharacter ' ')
