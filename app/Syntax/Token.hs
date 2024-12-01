module Syntax.Token (Token (..)) where

import Data.Text (Text)
import Syntax.Infix (InfixOp)

data Token
  = -- delimiters
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_CURLY
  | RIGHT_CURLY
  | LEFT_SQUARE
  | RIGHT_SQUARE
  | EOI
  | -- Symbols
    BACKSLASH
  | ASTERIK
  | COLON
  | SEMICOLON
  | COMMA
  | ASSIGN
  | EXCLAMATION
  | BAR
  | PERIOD
  | UNDERSCORE
  | ARROW
  | -- primitives
    TInt Int
  | TFloat Double
  | TChar Char
  | TString Text
  | TBinop InfixOp
  | Identifier Text
  | ConstIdent Text
  | -- keywords
    TYPE
  | VIEW
  | LET
  | CONST
  | EXTERN
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | FN
  deriving (Eq, Show)
