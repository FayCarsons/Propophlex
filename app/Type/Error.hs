module Type.Error (Error (..), parserError) where

import Data.Text (Text)
import Type.Type

data Error
  = UnknownType Text
  | UnboundVar Text
  | Expected Text Text
  | CannotErase Type
  | Parser Int Int Text
  deriving (Show, Eq)

parserError :: Int -> Int -> Text -> Error
parserError = Parser
