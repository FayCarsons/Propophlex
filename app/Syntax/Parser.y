{
module Syntax.Parser() where

import qualified Syntax.Token as T
}

%name propophlex
%tokentype { T.Token }
%error { ParseError }
%token 
  '('   { T.LEFT_PAREN }
  ')'  { T.RIGHT_PAREN }
  '{'   { T.LEFT_CURLY }
  '}'  { T.RIGHT_CURLY }
  '['  { T.LEFT_SQUARE }
  ']' { T.RIGHT_SQUARE }
  eoi          { T.EOI }
  '+'         { T.PLUS }
  '-'        { T.MINUS }
  '/'    { T.BACKSLASH }
  '*'     { T.ASTERIK }
  '%'      { T.PERCENT }
  '@'           { T.AT }
  ':'        { T.COLON }
  ';'    { T.SEMICOLON }
  ','        { T.COMMA }
  '='       { T.ASSIGN }
  eq           { T.EQ }
  neq          { T.NEQ }
  lt           { T.LT }
  lte          { T.LTE }
  gt           { T.GT }
  gte          { T.GTE }
  '!'  { T.EXCLAMATION }
  arrow        { T.ARROW }
  int          { T.TInt $$ }
  float        { T.TFloat $$ }
  char         { T.TChar $$ }
  string       { T.TString $$ }
  identifier   { T.Identifier $$ }
  static       { T.Static $$ }
  type         { T.TYPE }
  view         { T.VIEW }
  let          { T.LET }
  if           { T.IF }
  then         { T.THEN }
  else         { T.ELSE }
  match        { T.MATCH }
  with         { T.WITH }

%%


