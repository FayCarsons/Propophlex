module Syntax.Ast (
  Ast (..),
  int,
  float,
  char,
  string,
  typeVar,
  typeApplication,
  typeConcrete,
  letDeclaration,
  variable,
  typedArg,
  untypedArg,
  OpBinary (..),
  OpUnary (..),
  TypeRef (..),
  Signature (..),
) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Literal
  = LInt Int
  | LFloat Double
  | LChar Char
  | LString ByteString
  deriving (Eq, Show)

data Identifier = Identifier ByteString
  deriving (Eq, Show)

data Var = Var ByteString
  deriving (Eq, Show)

data Const = Const ByteString
  deriving (Eq, Show)

data TypeRef
  = ConcreteT Const
  | ApplicationT Const [Var]
  | VarT Var
  deriving (Eq, Show)

typeVar :: ByteString -> TypeRef
typeVar s = VarT $ Var s

typeApplication :: ByteString -> [ByteString] -> TypeRef
typeApplication t vs = ApplicationT (Const t) (map Var vs)

typeConcrete :: ByteString -> TypeRef
typeConcrete t = ConcreteT $ Const t

data Signature
  = LiteralT TypeRef
  | FnT [TypeRef]
  deriving (Eq, Show)

data OpBinary
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data OpUnary = Neg
  deriving (Eq, Show)

data Arg
  = Arg Var
  | Typed Var TypeRef
  deriving (Eq, Show)

untypedArg :: ByteString -> Arg
untypedArg s = Arg $ Var s

typedArg :: ByteString -> TypeRef -> Arg
typedArg s t = Typed (Var s) t

data Ast
  = Literal Literal
  | Variable Var
  | BinaryOp OpBinary Ast Ast
  | UnaryOp OpUnary Ast
  | Let Identifier (Maybe Signature) [Ast]
  | Lambda [Arg] [Ast]
  | Constant Const Signature Ast
  deriving (Eq, Show)

variable :: ByteString -> Ast
variable name = Variable $ Var name

letDeclaration :: ByteString -> (Maybe Signature) -> [Ast] -> Ast
letDeclaration name sig expr = Let (Identifier name) sig expr

int :: Int -> Ast
int n = Literal $ LInt n

float :: Double -> Ast
float n = Literal $ LFloat n

char :: Char -> Ast
char c = Literal $ LChar c

string :: ByteString -> Ast
string s = Literal $ LString s
