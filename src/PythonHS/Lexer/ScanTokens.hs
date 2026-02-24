module PythonHS.Lexer.ScanTokens (scanTokens) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import PythonHS.Lexer.LexerError (LexerError (UnexpectedCharacter))
import PythonHS.Lexer.Token (Token (Token))
import PythonHS.Lexer.Position (Position (Position))
import PythonHS.Lexer.TokenType
  ( TokenType
      ( AssignToken,
        PlusAssignToken,
        MinusAssignToken,
        StarAssignToken,
        SlashAssignToken,
        PercentAssignToken,
        DoubleSlashAssignToken,
        ColonToken,
        CommaToken,
        DefToken,
        EOFToken,
        ElseToken,
        ElifToken,
        TrueToken,
        FalseToken,
        NoneToken,
        ForToken,
        IdentifierToken,
        IfToken,
        InToken,
        IntegerToken,
        StringToken,
        LParenToken,
        MinusToken,
        NewlineToken,
        IndentToken,
        DedentToken,
        PlusToken,
        PrintToken,
        RParenToken,
        ReturnToken,
        BreakToken,
        ContinueToken,
        GlobalToken,
        PassToken,
        EqToken,
        NotEqToken,
        LtToken,
        GtToken,
        LteToken,
        GteToken,
        AndToken,
        OrToken,
        NotToken,
        LBracketToken,
        RBracketToken,
        LBraceToken,
        RBraceToken,
        SlashToken,
        PercentToken,
        StarToken,
        WhileToken
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

    go (c : rest) ln col False indentStack acc
      | isSpace c = go rest ln (col + 1) False indentStack acc
      | c == '=' =
          case rest of
            ('=' : rest') ->
              let tok = Token EqToken "==" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token AssignToken "=" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '!' =
          case rest of
            ('=' : rest') ->
              let tok = Token NotEqToken "!=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ -> Left (UnexpectedCharacter '!')
      | c == '<' =
          case rest of
            ('=' : rest') ->
              let tok = Token LteToken "<=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token LtToken "<" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '>' =
          case rest of
            ('=' : rest') ->
              let tok = Token GteToken ">=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token GtToken ">" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '+' =
          case rest of
            ('=' : rest') ->
              let tok = Token PlusAssignToken "+=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token PlusToken "+" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '-' =
          case rest of
            ('=' : rest') ->
              let tok = Token MinusAssignToken "-=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token MinusToken "-" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '*' =
          case rest of
            ('=' : rest') ->
              let tok = Token StarAssignToken "*=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token StarToken "*" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '/' =
          case rest of
            ('/' : '=' : rest') ->
              let tok = Token DoubleSlashAssignToken "//=" (Position ln col)
               in go rest' ln (col + 3) False indentStack (tok : acc)
            ('=' : rest') ->
              let tok = Token SlashAssignToken "/=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token SlashToken "/" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '%' =
          case rest of
            ('=' : rest') ->
              let tok = Token PercentAssignToken "%=" (Position ln col)
               in go rest' ln (col + 2) False indentStack (tok : acc)
            _ ->
              let tok = Token PercentToken "%" (Position ln col)
               in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '(' =
          let tok = Token LParenToken "(" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == ')' =
          let tok = Token RParenToken ")" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '[' =
          let tok = Token LBracketToken "[" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == ']' =
          let tok = Token RBracketToken "]" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '{' =
          let tok = Token LBraceToken "{" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '}' =
          let tok = Token RBraceToken "}" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == ':' =
          let tok = Token ColonToken ":" (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == ',' =
          let tok = Token CommaToken "," (Position ln col)
           in go rest ln (col + 1) False indentStack (tok : acc)
      | c == '"' =
          let (strContent, tailInput) = span (\x -> x /= '"' && x /= '\n') rest
              len = length strContent
           in case tailInput of
                ('"' : rest') ->
                  let tok = Token StringToken strContent (Position ln col)
                   in go rest' ln (col + len + 2) False indentStack (tok : acc)
                _ -> Left (UnexpectedCharacter '"')
      | isDigit c =
          let (digits, tailInput) = span isDigit (c : rest)
              len = length digits
              tok = Token IntegerToken digits (Position ln col)
           in go tailInput ln (col + len) False indentStack (tok : acc)
      | isAlpha c || c == '_' =
          let (word, tailInput) = span (\x -> isAlphaNum x || x == '_') (c : rest)
              len = length word
              tok = Token (keywordOrIdentifier word) word (Position ln col)
           in go tailInput ln (col + len) False indentStack (tok : acc)
      | otherwise = Left (UnexpectedCharacter c)

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

    keywordOrIdentifier :: String -> TokenType
    keywordOrIdentifier value
      | value == "print" = PrintToken
      | value == "if" = IfToken
      | value == "elif" = ElifToken
      | value == "True" = TrueToken
      | value == "False" = FalseToken
      | value == "None" = NoneToken
      | value == "else" = ElseToken
      | value == "while" = WhileToken
      | value == "for" = ForToken
      | value == "in" = InToken
      | value == "def" = DefToken
      | value == "return" = ReturnToken
      | value == "break" = BreakToken
      | value == "continue" = ContinueToken
      | value == "global" = GlobalToken
      | value == "pass" = PassToken
      | value == "and" = AndToken
      | value == "or" = OrToken
      | value == "not" = NotToken
      | otherwise = IdentifierToken
