module Syntax.Token (Token (..)) where

import Data.ByteString.Lazy.Char8 (ByteString)
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
  | TString ByteString
  | TBinop InfixOp
  | Identifier ByteString
  | ConstIdent ByteString
  | -- keywords
    TYPE
  | VIEW
  | LET
  | CONST
  | IF
  | THEN
  | ELSE
  | MATCH
  | WITH
  | FN
  deriving (Eq, Show)
