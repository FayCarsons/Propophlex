module Syntax.Ast (
  Ast (..),
  int,
  float,
  char,
  string,
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
  binaryOp,
  unaryOp,
  call,
  lambda,
  lambdaMatch,
  letDeclaration,
  ifThen,
  ifThenElse,
  match,
  variable,
  typedArg,
  untypedArg,
  sumLiteralArg,
  erasedArg,
  sumTypeDeclaration,
  recordTypeDeclaration,
  fieldAccess,
  OpBinary (..),
  OpUnary (..),
  TypeRef (..),
  Identifier (..),
) where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

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
  | LUnit
  | LRecord [(Identifier, Ast annotation)]
  | LSum Const [Ast annotation]
  deriving (Eq, Show)

unitLiteral :: Literal annotation
unitLiteral = LUnit

recordLiteral :: [(ByteString, Ast annotation)] -> Literal annotation
recordLiteral =
  LRecord
    . map (first Identifier)

sumLiteral :: ByteString -> [Ast annotation] -> Literal annotation
sumLiteral variant = LSum (Const variant)

int :: Int -> Literal annotation
int = LInt

float :: Double -> Literal annotation
float = LFloat

char :: Char -> Literal annotation
char = LChar

string :: ByteString -> Literal annotation
string = LString

literal :: Literal annotation -> Ast annotation
literal = Literal

data TypeRef
  = ConcreteT Const
  | ApplicationT Const [TypeRef]
  | FnT [TypeRef]
  | VarT Var
  | Unit
  deriving (Eq, Show)

typeVar :: ByteString -> TypeRef
typeVar s = VarT $ Var s

typeSigError unexpected = error $ "Ast.typeApplication: first type must be a concrete type. Got: " ++ show unexpected

typeApplication :: [TypeRef] -> TypeRef
typeApplication types = case types of
  ConcreteT (Const constructor) : args -> ApplicationT (Const constructor) args
  unexpected -> typeSigError unexpected

typeConcrete :: ByteString -> TypeRef
typeConcrete t = ConcreteT $ Const t

fnType :: [TypeRef] -> TypeRef
fnType = FnT

data OpBinary
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

binaryOp :: OpBinary -> Ast () -> Ast () -> Ast ()
binaryOp = BinaryOp ()

data OpUnary = Neg
  deriving (Eq, Show)

unaryOp :: OpUnary -> Ast () -> Ast ()
unaryOp = UnaryOp ()

data Arg
  = Arg Var
  | Typed Var TypeRef
  | SumLiteral Const [Identifier]
  | Erased
  deriving (Eq, Show)

untypedArg :: ByteString -> Arg
untypedArg s = Arg $ Var s

typedArg :: ByteString -> TypeRef -> Arg
typedArg s = Typed (Var s)

sumLiteralArg :: ByteString -> [ByteString] -> Arg
sumLiteralArg variant fields = SumLiteral (Const variant) (map Identifier fields)

erasedArg :: Arg
erasedArg = Erased

data TypeDeclaration
  = RecordTypeDeclaration Const (Maybe [TypeRef]) [(Var, TypeRef)]
  | SumTypeDeclaration Const (Maybe [TypeRef]) [(Const, TypeRef)]
  deriving (Eq, Show)

recordTypeDeclaration :: [TypeRef] -> [(ByteString, TypeRef)] -> Ast ()
recordTypeDeclaration types fields = DeclarationT () $ case types of
  [ConcreteT cons] -> RecordTypeDeclaration cons Nothing (map (first Var) fields)
  ConcreteT cons : typeParams -> RecordTypeDeclaration cons (Just $ reverse typeParams) (map (first Var) fields)
  unexpected -> typeSigError unexpected

fieldAccess :: ByteString -> ByteString -> Ast ()
fieldAccess v field = FieldAccess () (Identifier v) (Identifier field)

sumTypeDeclaration :: [TypeRef] -> [(ByteString, TypeRef)] -> Ast ()
sumTypeDeclaration types variants = case types of
  [ConcreteT cons] -> DeclarationT () $ SumTypeDeclaration cons Nothing (map (first Const) variants)
  ConcreteT cons : typeParams -> DeclarationT () $ SumTypeDeclaration cons (Just $ reverse typeParams) (map (first Const) variants)
  unexpected -> typeSigError unexpected

data FunctionApplication annotation
  = FunctionApplication (Ast annotation) [Ast annotation]
  deriving (Eq, Show)

data Ast annotation
  = Literal (Literal annotation)
  | Erase annotation [Ast annotation]
  | Variable annotation Identifier
  | FieldAccess annotation Identifier Identifier
  | DeclarationT annotation TypeDeclaration
  | BinaryOp annotation OpBinary (Ast annotation) (Ast annotation)
  | UnaryOp annotation OpUnary (Ast annotation)
  | Let annotation Identifier (Maybe TypeRef) [Ast annotation]
  | Lambda annotation [Arg] [Ast annotation]
  | Constant annotation Const TypeRef (Ast annotation)
  | Call annotation (FunctionApplication annotation)
  | If annotation (Ast annotation) (Ast annotation) (Ast annotation)
  | Match annotation (Ast annotation) [(Literal annotation, Ast annotation)]
  deriving (Eq, Show)

ifThen :: Ast () -> Ast () -> Ast ()
ifThen p t = If () p t (Literal LUnit)

ifThenElse :: Ast () -> Ast () -> Ast () -> Ast ()
ifThenElse = If ()

match :: Ast () -> [(Literal (), Ast ())] -> Ast ()
match = Match ()

call :: Ast () -> Ast () -> Ast ()
call f xs = Call () (FunctionApplication f [xs])

lambda :: [Arg] -> [Ast ()] -> Ast ()
lambda = Lambda ()

lambdaMatch :: [(Literal (), Ast ())] -> Ast ()
lambdaMatch arms =
  Lambda () [untypedArg x] [Match () (variable x) arms]
 where
  x = BS.pack "x"

erase :: [Ast ()] -> Ast ()
erase = Erase ()

unitT :: TypeRef
unitT = Unit

variable :: ByteString -> Ast ()
variable name = Variable () $ Identifier name

letDeclaration :: ByteString -> Maybe TypeRef -> [Ast ()] -> Ast ()
letDeclaration name = Let () (Identifier name)
