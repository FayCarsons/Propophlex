module Type.Error (Error (..)) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Error
  = UnknownType ByteString
  | UnboundVar ByteString
  | Expected ByteString ByteString
