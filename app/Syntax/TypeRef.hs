module Syntax.TypeRef (TypeRef (..)) where

import Data.Text (Text)

data TypeRef
  = ConcreteT Text
  | ApplicationT Text [TypeRef]
  | FnT [TypeRef]
  | VarT Text
  | AnonymousRecord [(Text, TypeRef)]
  | UnitT
  deriving (Eq, Show)
