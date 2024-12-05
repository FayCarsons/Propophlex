module Type.Type (Type (..), Primitive (..), int, uint, float, char, string, bool, lambda, tuple, array, unit, toText) where

import Data.Text (Text)
import qualified Data.Text as Text

data Primitive
  = Int
  | UInt
  | Float
  | Char
  | String
  | Bool
  | Lambda [Type]
  | Tuple Int [Type]
  | Array Int Type
  | Unit
  deriving (Eq, Show)

data Type
  = Unsolved
  | Primitive Primitive
  | Defined Text
  | TypeApplication Type [Type]
  | Skolem Int -- A skolem , a hole for monomorphization
  | TypeVar Int -- Always bound by `forall` - this should maybe just be an AST node?
  | UnificationVar Int -- Placeholders that stand for as of yet unknown concrete types
  | TypeError Text
  deriving (Eq, Show)

int :: Type
int = Primitive Int

uint :: Type
uint = Primitive UInt

float :: Type
float = Primitive Float

char :: Type
char = Primitive Char

string :: Type
string = Primitive String

bool :: Type
bool = Primitive Bool

lambda :: [Type] -> Type
lambda = Primitive . Lambda

tuple :: Int -> [Type] -> Type
tuple n ts = Primitive $ Tuple n ts

array :: Int -> Type -> Type
array n t = Primitive $ Array n t

unit :: Type
unit = Primitive Unit

toText :: Type -> Text
toText = Text.show
