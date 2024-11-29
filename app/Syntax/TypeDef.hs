module Syntax.TypeDef (TypeDefinition (..), ADT (..)) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Syntax.TypeRef (TypeRef)

data ADT
  = Sum
  | Record
  deriving (Eq, Show)

data TypeDefinition
  = TypeDefinition
  { name :: ByteString
  , kind :: ADT
  , parameters :: Maybe [TypeRef]
  , body :: [(ByteString, TypeRef)]
  }
  deriving (Eq, Show)
