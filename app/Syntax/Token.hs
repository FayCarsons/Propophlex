module Syntax.Token (Token (..)) where

import Data.ByteString.Lazy.Char8 (ByteString)

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
    PLUS
  | MINUS
  | BACKSLASH
  | ASTERIK
  | PERCENT
  | AT
  | COLON
  | SEMICOLON
  | COMMA
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | EXCLAMATION
  | BAR
  | PERIOD
  | UNDERSCORE
  | ARROW
  | -- primitives
    TInt Int
  | TFloat Double
  | TChar Char
  | TString ByteString
  | Identifier ByteString
  | Static ByteString
  | -- keywords
    TYPE
  | VIEW
  | LET
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | FN
  deriving (Eq, Show)
