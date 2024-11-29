module Type.Type (Type (..), Primitive (..), int, uint, float, char, string, bool, lambda, tuple, array, unit) where

import Data.ByteString.Lazy.Char8 (ByteString)

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
  | Defined ByteString
  | Skolem Int -- A skolem
  | TypeVar Int -- Always bound by `forall`
  | UnificationVar Int -- Placeholders that stand for as of yet unknown concrete types
  | TypeError ByteString
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
