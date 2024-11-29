module Syntax.TypeRef (TypeRef (..)) where

import Data.ByteString.Lazy.Char8 (ByteString)

data TypeRef
  = ConcreteT ByteString
  | ApplicationT [TypeRef]
  | FnT [TypeRef]
  | VarT ByteString
  | AnonymousRecord [(ByteString, TypeRef)]
  | UnitT
  deriving (Eq, Show)
