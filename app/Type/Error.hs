module Type.Error (Error (..)) where

import Data.Text (Text)
import Type.Type

data Error
  = UnknownType Text
  | UnboundVar Text
  | Expected Text Text
  | CannotErase Type
