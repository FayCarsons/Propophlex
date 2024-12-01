module Type.Error (Error (..)) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Type.Type

data Error
  = UnknownType ByteString
  | UnboundVar ByteString
  | Expected ByteString ByteString
  | CannotErase Type
