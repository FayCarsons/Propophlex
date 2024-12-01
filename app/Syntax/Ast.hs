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
  externDeclaration,
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
  isUnsolved,
  Const (..),
  Var (..),
  Identifier (..),
  Literal (..),
  Application (..),
) where

import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import qualified Data.Text as Text
import Syntax.Infix (InfixOp)
import Syntax.TypeDef (ADT (..), TypeDefinition (..))
import Syntax.TypeRef (TypeRef (..))
import Type.Type (Type (..))
import qualified Type.Type as Type

newtype Identifier = Identifier Text
  deriving (Eq, Show)

newtype Var = Var Text
  deriving (Eq, Show)

newtype Const = Const Text
  deriving (Eq, Show)

data Literal annotation
  = LInt Int
  | LFloat Double
  | LChar Char
  | LString Text
  | LBool Bool
  | LUnit
  | LRecord [(Identifier, Ast annotation)]
  | LSum Const [Ast annotation]
  | LTuple [Literal annotation]
  deriving (Eq, Show)

unitLiteral :: Literal Type
unitLiteral = LUnit

recordLiteral :: [(Text, Ast Type)] -> Literal Type
recordLiteral =
  LRecord
    . map (first Identifier)

sumLiteral :: Text -> [Ast Type] -> Literal Type
sumLiteral variant = LSum (Const variant)

tuple :: [Literal Type] -> Literal Type
tuple = LTuple

int :: Int -> Literal Type
int = LInt

float :: Double -> Literal Type
float = LFloat

char :: Char -> Literal Type
char = LChar

string :: Text -> Literal Type
string = LString

literal :: Literal Type -> Ast Type
literal = \case
  LInt n -> Literal Type.int (LInt n)
  LFloat n -> Literal Type.float (LFloat n)
  LChar c -> Literal Type.char (LChar c)
  LString s -> Literal Type.string (LString s)
  LUnit -> Literal Type.unit LUnit
  other -> Literal Type.Unsolved other

typeVar :: Text -> TypeRef
typeVar = VarT

typeSigError label unexpected = error $ label ++ ": first type must be a concrete type. Got: " ++ show unexpected

typeApplication :: [TypeRef] -> TypeRef
typeApplication types = case types of
  ConcreteT constructor : args -> ApplicationT constructor $ reverse args
  ts@((ApplicationT t _) : _) -> ApplicationT t ts
  unexpected -> typeSigError "typeApplication" unexpected

typeConcrete :: Text -> TypeRef
typeConcrete = ConcreteT

fnType :: [TypeRef] -> TypeRef
fnType = FnT

anonymousRecordT :: [(Text, TypeRef)] -> TypeRef
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

untypedArg :: Text -> Arg
untypedArg s = Arg $ Var s

typedArg :: Text -> TypeRef -> Arg
typedArg = Typed . Var

sumLiteralArg :: Text -> [Text] -> Arg
sumLiteralArg variant fields = SumLiteral (Const variant) (map Identifier fields)

erasedArg :: Arg
erasedArg = Erased

recordTypeDeclaration :: [TypeRef] -> [(Text, TypeRef)] -> Ast Type
recordTypeDeclaration params fields = TypeDef $ case params of
  [ConcreteT cons] -> TypeDefinition cons Record Nothing fields
  ConcreteT cons : typeParams -> TypeDefinition cons Record (Just $ reverse typeParams) fields
  unexpected -> typeSigError "recordTypeDeclaration" unexpected

fieldAccess :: Text -> Text -> Ast Type
fieldAccess v field = FieldAccess Unsolved (Identifier v) (Identifier field)

sumTypeDeclaration :: [TypeRef] -> [(Text, TypeRef)] -> Ast Type
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
  | Extern annotation Identifier TypeRef Text
  | Call annotation (Application annotation)
  | Match annotation (Ast annotation) [(Pattern annotation, Ast annotation)]
  deriving (Eq, Show)

withType :: Type -> Ast Type -> Ast Type
withType t = \case
  Literal _ lit -> Literal t lit
  Erase _ exprs -> Erase t exprs
  Variable _ ident -> Variable t ident
  Infix _ op -> Infix t op
  FieldAccess _ ident field -> FieldAccess t ident field
  TypeDef td -> TypeDef td
  Let _ name sig exprs -> Let t name sig exprs
  Fn _ args body -> Fn t args body
  Constant _ name sig body -> Constant t name sig body
  Extern _ name sig symbol -> Extern t name sig symbol
  Call _ app -> Call t app
  Match _ body arms -> Match t body arms

typeOf :: Ast Type -> Type
typeOf = \case
  Literal t _ -> t
  Erase t _ -> t
  Variable t _ -> t
  Infix t _ -> t
  FieldAccess t _ _ -> t
  TypeDef (TypeDefinition{name}) -> Defined name
  Let t _ _ _ -> t
  Fn t _ _ -> t
  Constant t _ _ _ -> t
  Extern t _ _ _ -> t
  Call t _ -> t
  Match t _ _ -> t

isUnsolved :: Ast Type -> Bool
isUnsolved =
  \case
    Unsolved -> True
    _ -> False
    . typeOf

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

matchBinding :: Text -> Pattern Type
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
  x = Text.pack "x"

erase :: Ast Type -> Ast Type
erase = Erase Unsolved

unitT :: TypeRef
unitT = UnitT

variable :: Text -> Ast Type
variable = Variable Unsolved . Identifier

letDeclaration :: Text -> Maybe TypeRef -> [Ast Type] -> Ast Type
letDeclaration = Let Unsolved . Identifier

constDeclaration :: Text -> TypeRef -> Ast Type -> Ast Type
constDeclaration = Constant Unsolved . Const

externDeclaration :: Text -> TypeRef -> Text -> Ast Type
externDeclaration = Extern Unsolved . Identifier
