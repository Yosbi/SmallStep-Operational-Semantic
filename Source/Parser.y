{

module Parser (parser, lexer) where
import Data.Char (isAlpha, isDigit, isSpace)
import Interpreter (Expression(..), Statement(..))

}

%name parser
%tokentype { Token }
%error { parseError }

%token
      str             { TokenVar $$ }
      int             { TokenInt $$ }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '('             { TokenOB }
      ')'             { TokenCB }
      '['             { TokenOBK }
      ']'             { TokenCBK }
      '='             { TokenEQ }
      ';'             { TokenSC }
      par             { TokenPar }
      skip             { TokenSkip }
%%



Statement :  str '=' Expression  {Assing $1 $3}
        | '[' stmts ']' {Sequence (reverseList  $2)}
        | '[' stmts ']' par '[' stmts ']' {Par (reverseList  $2) (reverseList $6)}
        | Statement par '[' stmts ']' {Par ([$1]) (reverseList $4)}
        | '[' stmts ']' par Statement {Par (reverseList  $2) ([$5])}
        | Statement par Statement {Par ([$1]) ([$3])}
        | skip {Skip}

Expression  : str               { Var $1 }
      | int                     { Num $1 }
      | '(' Expression '+' Expression ')'   { Sum $2 $4 }
      | '(' Expression '-' Expression ')'   { Diff $2 $4 }
      | '(' Expression '*' Expression ')'   { Mult $2 $4 }

stmts : stmts ';' Statement          { $3 : $1 }
      | stmts ';'               { $1 }
      | Statement			{ [$1] }
      | {- empty -}		{ [] }

{

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

parseError :: [Token] -> a
parseError _ = error "Parse error"
        
data Token
      = TokenVar String
      | TokenInt Int
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenOB
      | TokenCB
      | TokenOBK
      | TokenCBK
      | TokenEQ
      | TokenSC
      | TokenPar
      | TokenSkip
 deriving Show
          
lexer :: String -> [Token]
lexer [] = []
lexer ('p':'a':'r':cs) = TokenPar : lexer cs
lexer ('s':'k':'i':'p':cs) = TokenSkip : lexer cs
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('[':cs) = TokenOBK : lexer cs
lexer (']':cs) = TokenCBK : lexer cs
lexer ('=':cs) = TokenEQ : lexer cs
lexer (';':cs) = TokenSC : lexer cs
lexNum cs = TokenInt (read num) : lexer rest
  where (num,rest) = span isDigit cs

lexVar cs = TokenVar var : lexer rest
  where (var,rest) = span isAlpha cs

}
