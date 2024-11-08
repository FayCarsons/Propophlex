module Syntax.Ast () where

data Literal
  = LInt Int
  | LFloat Double
  | LChar Char
  | LString

data Identifier
  = Var -- Variable, top-level fn names
  | Const -- Types, effects, constants

data Expr

data ControlFlow
  = If Expr Expr Expr
  | Match Expr [(Expr, Expr)]
