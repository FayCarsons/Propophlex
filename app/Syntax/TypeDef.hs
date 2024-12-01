module Syntax.TypeDef (TypeDefinition (..), ADT (..)) where

import Data.Text (Text)
import Syntax.TypeRef (TypeRef)

data ADT
  = Sum
  | Record
  deriving (Eq, Show)

data TypeDefinition
  = TypeDefinition
  { name :: Text
  , kind :: ADT
  , parameters :: Maybe [TypeRef]
  , body :: [(Text, TypeRef)]
  }
  deriving (Eq, Show)
