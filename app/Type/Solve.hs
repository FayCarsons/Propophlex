{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Type.Solve (infer) where

import Control.Monad.Reader
import Data.IORef as Ref

-- import Data.Map.Strict (Map)

import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Syntax.Ast (Ast (..))
import qualified Syntax.Ast as Ast
import Syntax.Infix (InfixOp (..))
import Syntax.TypeDef (ADT (..), TypeDefinition (..))
import Syntax.TypeRef (TypeRef (..))
import Type.Context (ContextM)
import qualified Type.Context as Ctx
import qualified Type.Error as Error
import Type.Type (Primitive (..), Type (..))
import qualified Type.Type as Type
import Prelude hiding (EQ, GT, LT)

eraseable :: Type -> Bool
eraseable = const True

operatorTypes :: Map InfixOp [TypeRef]
operatorTypes =
  Map.fromList
    [ (Plus, binop)
    , (Minus, binop)
    , (Mul, binop)
    , (Div, binop)
    , (GT, comparison)
    , (LT, comparison)
    , (GTE, comparison)
    , (LTE, comparison)
    , (EQ, comparison)
    , (NEQ, comparison)
    , (And, boolean)
    , (Or, boolean)
    , (Xor, boolean)
    , (Concat, monoid)
    , (Append, monoid) -- One of these should technically be referred to as a semigroup :p
    , (Cons, cons)
    ]
 where
  binop = replicate 3 (VarT "a")
  comparison = replicate 2 (VarT "a") ++ [ConcreteT "Bool"]
  boolean = replicate 3 (ConcreteT "Bool")
  monoid = replicate 3 (ApplicationT "List" [VarT "a"])
  cons = VarT "a" : replicate 2 (ApplicationT "List" [VarT "a"])

uniqueTypeVars :: [TypeRef] -> [Text]
uniqueTypeVars = go
 where
  go (VarT tv : ts) = tv : go ts
  go (ApplicationT _ ts : xs) = go ts ++ go xs
  go _ = []

typeRefToType :: TypeRef -> ContextM Type
typeRefToType = \case
  ConcreteT typename -> return $ Defined typename
  ApplicationT typename _ -> return $ Defined typename
  FnT ts -> Primitive . Lambda <$> mapM typeRefToType ts
  VarT _ -> return Unsolved
  UnitT -> return $ Primitive Unit
  _ -> error "uwu"

insertTypeVars :: [(Text, Int)] -> [TypeRef] -> [Type]
insertTypeVars mappings = go
 where
  go (VarT tv : ts) = case lookup tv mappings of
    Just var -> TypeVar var : go ts
    Nothing -> error "This should be unreachable"
  -- NOTE: May need to change this from defined once we can have type sigs like:
  -- `return : a -> m a`
  go (ApplicationT t ts : xs) = TypeApplication (Defined t) (go ts) : go xs
  go _ = error "This should definitely be unreachable wtf"

signatureInfix :: InfixOp -> ContextM Type
signatureInfix inf = case Map.lookup inf operatorTypes of
  Just sig -> do
    let vs = uniqueTypeVars sig
    tvs <- traverse (\tv -> (tv,) <$> Ctx.freshUVar) vs
    let ts = insertTypeVars tvs sig
    return $ Primitive $ Lambda ts
  Nothing -> error $ "Unknown operator: " ++ show inf

infer :: Ast Type -> ContextM (Ast Type)
infer node = case node of
  -- NOTE: Literal should always be typed from AST construction
  Literal _ _ -> return node
  -- NOTE: Erase should probably use its signature lol
  Erase _ _ erasedBody -> infer erasedBody
  Variable _ (Ast.Identifier varName) -> do
    scope <- asks Ctx.scope >>= liftIO . Ref.readIORef
    case Map.lookup varName scope of
      Just ty -> return $ Ast.withType ty node
      Nothing -> do
        varID <- Ctx.freshUVar
        let ty = UnificationVar varID
        Ctx.registerScope varName ty
        return $ Ast.withType ty node
  Infix _ op -> flip Infix op <$> signatureInfix op
  FieldAccess _ (Ast.Identifier var) (Ast.Identifier field) -> do
    Ctx.lookupScope var >>= \case
      -- TODO: Is `Defined` what we want here?
      Just (Defined typename) ->
        Ctx.lookupType typename >>= \case
          Just TypeDefinition{name, kind, body} ->
            if kind == Record
              then do
                case lookup field body of
                  Just fieldType -> flip Ast.withType node <$> typeRefToType fieldType
                  Nothing -> do
                    let msg = "Type " <> name <> " does not include field " <> field
                    tell [Error.UnboundVar msg]
                    return $ FieldAccess (TypeError msg) (Ast.Identifier var) (Ast.Identifier field)
              else do
                let msg = "Type '" <> name <> "' is not a record and does not contain the field '" <> field <> "'"
                tell [Error.UnboundVar msg]
                return $ FieldAccess (TypeError msg) (Ast.Identifier var) (Ast.Identifier field)
          Nothing -> undefined
      _ -> do
        let msg = "Unknown variable: '" <> var <> "'"
        tell [Error.UnboundVar msg]
        return $ FieldAccess (TypeError msg) (Ast.Identifier var) (Ast.Identifier field)
  TypeDef typeDef@TypeDefinition{name} -> Ctx.registerType name typeDef $> node
  Let _ name ann body -> do
    ts <- mapM infer body
    annotation <- undefined -- typeRefToType <$> ann
    let retVal = last ts
    undefined
  {-
  case annotation of
    Just expectedType ->
      if expectedType == retVal
        then
          return $ Let expectedType name ann ts
        else do
          tell [Error.Expected (Type.toByteString expectedType) (Type.toByteString retVal)]
          return node
   - -}
  Fn _ args body -> undefined
  Constant _ name sig body -> undefined
  Call _ app -> undefined
  Match _ expr arms -> undefined
  _ -> undefined
