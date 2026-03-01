module PythonHS.Lexer.KeywordOrIdentifier (keywordOrIdentifier) where

import PythonHS.Lexer.TokenType
  ( TokenType
      ( AndToken,
        AsToken,
        BreakToken,
        ContinueToken,
        DefToken,
        ElifToken,
        ElseToken,
        ExceptToken,
        FinallyToken,
        FalseToken,
        ForToken,
        FromToken,
        GlobalToken,
        IdentifierToken,
        IfToken,
        ImportToken,
        InToken,
        NoneToken,
        NotToken,
        OrToken,
        PassToken,
        PrintToken,
        RaiseToken,
        ReturnToken,
        TrueToken,
        TryToken,
        WhileToken
      )
  )

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
  | value == "from" = FromToken
  | value == "as" = AsToken
  | value == "import" = ImportToken
  | value == "try" = TryToken
  | value == "except" = ExceptToken
  | value == "finally" = FinallyToken
  | value == "raise" = RaiseToken
  | value == "and" = AndToken
  | value == "or" = OrToken
  | value == "not" = NotToken
  | otherwise = IdentifierToken
