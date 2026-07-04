module PythonHS.Lexer.KeywordOrIdentifier (keywordOrIdentifier) where

import PythonHS.Lexer.TokenType
  ( TokenType
      ( AndToken,
        AsToken,
        BreakToken,
        CaseToken,
        ClassToken,
        ContinueToken,
        DefToken,
        ElifToken,
        ElseToken,
        ExceptToken,
        FalseToken,
        FinallyToken,
        ForToken,
        FromToken,
        GlobalToken,
        IdentifierToken,
        IfToken,
        ImportToken,
        InToken,
        LambdaToken,
        MatchToken,
        NoneToken,
        NotToken,
        OrToken,
        PassToken,
        PrintToken,
        RaiseToken,
        ReturnToken,
        TrueToken,
        TryToken,
        WhileToken,
        WithToken,
        YieldToken
      ),
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
  | value == "class" = ClassToken
  | value == "lambda" = LambdaToken
  | value == "yield" = YieldToken
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
  | value == "match" = MatchToken
  | value == "case" = CaseToken
  | value == "and" = AndToken
  | value == "or" = OrToken
  | value == "not" = NotToken
  | value == "with" = WithToken
  | otherwise = IdentifierToken
