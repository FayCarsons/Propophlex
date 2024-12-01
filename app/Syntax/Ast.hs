{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Syntax.Ast (
  Ast (..),
  int,
  float,
  char,
  string,
  tuple,
  unitT,
  unitLiteral,
  recordLiteral,
  sumLiteral,
  literal,
  erase,
  typeVar,
  typeApplication,
  typeConcrete,
  fnType,
  binaryInfix,
  unary,
  call,
  lambda,
  lambdaMatch,
  letDeclaration,
  constDeclaration,
  ifThen,
  ifThenElse,
  match,
  matchLiteral,
  matchBinding,
  matchErase,
  variable,
  typedArg,
  untypedArg,
  sumLiteralArg,
  erasedArg,
  sumTypeDeclaration,
  recordTypeDeclaration,
  anonymousRecordT,
  fieldAccess,
  withType,
  typeOf,
  Const (..),
  Var (..),
  Identifier (..),
  Literal (..),
  Application (..),
) where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Syntax.Infix (InfixOp)
import Syntax.TypeDef (ADT (..), TypeDefinition (..))
import Syntax.TypeRef (TypeRef (..))
import Type.Type (Type (..))
import qualified Type.Type as Type

newtype Identifier = Identifier ByteString
  deriving (Eq, Show)

newtype Var = Var ByteString
  deriving (Eq, Show)

newtype Const = Const ByteString
  deriving (Eq, Show)

data Literal annotation
  = LInt Int
  | LFloat Double
  | LChar Char
  | LString ByteString
  | LBool Bool
  | LUnit
  | LRecord [(Identifier, Ast annotation)]
  | LSum Const [Ast annotation]
  | LTuple [Literal annotation]
  deriving (Eq, Show)

unitLiteral :: Literal Type
unitLiteral = LUnit

recordLiteral :: [(ByteString, Ast Type)] -> Literal Type
recordLiteral =
  LRecord
    . map (first Identifier)

sumLiteral :: ByteString -> [Ast Type] -> Literal Type
sumLiteral variant = LSum (Const variant)

tuple :: [Literal Type] -> Literal Type
tuple = LTuple

int :: Int -> Literal Type
int = LInt

float :: Double -> Literal Type
float = LFloat

char :: Char -> Literal Type
char = LChar

string :: ByteString -> Literal Type
string = LString

literal :: Literal Type -> Ast Type
literal = \case
  LInt n -> Literal Type.int (LInt n)
  LFloat n -> Literal Type.float (LFloat n)
  LChar c -> Literal Type.char (LChar c)
  LString s -> Literal Type.string (LString s)
  LUnit -> Literal Type.unit LUnit
  other -> Literal Type.Unsolved other

typeVar :: ByteString -> TypeRef
typeVar = VarT

typeSigError label unexpected = error $ label ++ ": first type must be a concrete type. Got: " ++ show unexpected

typeApplication :: [TypeRef] -> TypeRef
typeApplication types = case types of
  ConcreteT constructor : args -> ApplicationT constructor $ reverse args
  ts@((ApplicationT t _) : _) -> ApplicationT t ts
  unexpected -> typeSigError "typeApplication" unexpected

typeConcrete :: ByteString -> TypeRef
typeConcrete = ConcreteT

fnType :: [TypeRef] -> TypeRef
fnType = FnT

anonymousRecordT :: [(ByteString, TypeRef)] -> TypeRef
anonymousRecordT = AnonymousRecord

binaryInfix :: InfixOp -> Ast Type -> Ast Type -> Ast Type
binaryInfix op l r = Call Unsolved (Application (Call Unsolved (Application (Infix Unsolved op) l)) r)

unary :: InfixOp -> Ast Type -> Ast Type
unary op l = Call Unsolved (Application (Infix Unsolved op) l)

data Arg
  = Arg Var
  | Typed Var TypeRef
  | SumLiteral Const [Identifier]
  | Erased
  deriving (Eq, Show)

untypedArg :: ByteString -> Arg
untypedArg s = Arg $ Var s

typedArg :: ByteString -> TypeRef -> Arg
typedArg = Typed . Var

sumLiteralArg :: ByteString -> [ByteString] -> Arg
sumLiteralArg variant fields = SumLiteral (Const variant) (map Identifier fields)

erasedArg :: Arg
erasedArg = Erased

recordTypeDeclaration :: [TypeRef] -> [(ByteString, TypeRef)] -> Ast Type
recordTypeDeclaration params fields = TypeDef $ case params of
  [ConcreteT cons] -> TypeDefinition cons Record Nothing fields
  ConcreteT cons : typeParams -> TypeDefinition cons Record (Just $ reverse typeParams) fields
  unexpected -> typeSigError "recordTypeDeclaration" unexpected

fieldAccess :: ByteString -> ByteString -> Ast Type
fieldAccess v field = FieldAccess Unsolved (Identifier v) (Identifier field)

sumTypeDeclaration :: [TypeRef] -> [(ByteString, TypeRef)] -> Ast Type
sumTypeDeclaration params variants = TypeDef $ case params of
  [ConcreteT cons] -> TypeDefinition cons Sum Nothing variants
  ConcreteT cons : typeParams -> TypeDefinition cons Sum (Just $ reverse typeParams) variants
  unexpected -> typeSigError "sumTypeDeclaration" unexpected

data Application annotation
  = Application (Ast annotation) (Ast annotation)
  deriving (Eq, Show)

data Ast annotation
  = Literal annotation (Literal annotation)
  | Erase annotation (Ast annotation)
  | Variable annotation Identifier
  | Infix annotation InfixOp
  | FieldAccess annotation Identifier Identifier
  | TypeDef TypeDefinition
  | Let annotation Identifier (Maybe TypeRef) [Ast annotation]
  | Fn annotation [Arg] [Ast annotation]
  | Constant annotation Const TypeRef (Ast annotation)
  | Call annotation (Application annotation)
  | Match annotation (Ast annotation) [(Pattern annotation, Ast annotation)]
  deriving (Eq, Show)

withType :: Type -> Ast Type -> Ast Type
withType t expr = case expr of
  Literal _ lit -> Literal t lit
  Erase _ exprs -> Erase t exprs
  Variable _ ident -> Variable t ident
  Infix _ op -> Infix t op
  FieldAccess _ ident field -> FieldAccess t ident field
  TypeDef td -> TypeDef td
  Let _ name sig exprs -> Let t name sig exprs
  Fn _ args body -> Fn t args body
  Constant _ name sig expr -> Constant t name sig expr
  Call _ app -> Call t app
  Match _ expr arms -> Match t expr arms

typeOf :: Ast Type -> Type
typeOf (Literal t _) = t
typeOf (Erase t _) = t
typeOf (Variable t _) = t
typeOf (Infix t _) = t
typeOf (FieldAccess t _ _) = t
typeOf (TypeDef (TypeDefinition{name})) = Defined name
typeOf (Let t _ _ _) = t
typeOf (Fn t _ _) = t
typeOf (Constant t _ _ _) = t
typeOf (Call t _) = t
typeOf (Match t _ _) = t

ifThen :: Ast Type -> Ast Type -> Ast Type
ifThen p t = Match Unsolved p [(LitP $ LBool True, t), (LitP $ LBool False, Literal (Primitive Type.Unit) LUnit)]

ifThenElse :: Ast Type -> Ast Type -> Ast Type -> Ast Type
ifThenElse p t f = Match Unsolved p [(LitP $ LBool True, t), (LitP $ LBool False, f)]

data Pattern annotation
  = LitP (Literal annotation)
  | BindP annotation Var
  | EraseP annotation
  deriving (Eq, Show)

match :: Ast Type -> [(Pattern Type, Ast Type)] -> Ast Type
match = Match Unsolved

matchLiteral :: Literal Type -> Pattern Type
matchLiteral = LitP

matchBinding :: ByteString -> Pattern Type
matchBinding = BindP Unsolved . Var

matchErase :: Pattern Type
matchErase = EraseP Unsolved

call :: Ast Type -> Ast Type -> Ast Type
call f xs = Call Unsolved $ Application f xs

lambda :: [Arg] -> [Ast Type] -> Ast Type
lambda = Fn Unsolved

lambdaMatch :: [(Pattern Type, Ast Type)] -> Ast Type
lambdaMatch arms =
  Fn Unsolved [untypedArg x] [Match Unsolved (variable x) arms]
 where
  x = BS.pack "x"

erase :: Ast Type -> Ast Type
erase = Erase Unsolved

unitT :: TypeRef
unitT = UnitT

variable :: ByteString -> Ast Type
variable = Variable Unsolved . Identifier

letDeclaration :: ByteString -> Maybe TypeRef -> [Ast Type] -> Ast Type
letDeclaration = Let Unsolved . Identifier

constDeclaration :: ByteString -> TypeRef -> Ast Type -> Ast Type
constDeclaration = Constant Unsolved . Const
