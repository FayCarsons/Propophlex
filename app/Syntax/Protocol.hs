module Syntax.Protocol () where

import Data.ByteString.Lazy.Char8 (ByteString)
import Syntax.TypeRef (TypeRef)

data Protocol
  = Protocol
  { name :: ByteString
  , parameters :: [ByteString]
  , signatures :: [(ByteString, TypeRef)]
  }
