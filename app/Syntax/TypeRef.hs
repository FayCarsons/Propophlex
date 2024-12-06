module Syntax.TypeRef (TypeRef (..), tupleType) where

import Data.Text (Text)

data TypeRef
  = ConcreteT Text
  | ApplicationT Text [TypeRef]
  | FnT [TypeRef]
  | VarT Text
  | AnonymousRecord [(Text, TypeRef)]
  | Tuple [TypeRef]
  | UnitT
  deriving (Eq, Show)

tupleType :: [TypeRef] -> TypeRef
tupleType = Tuple
