module Type.IR () where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data Expr
  = Ident ByteString
  | Foo
