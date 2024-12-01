{-# LANGUAGE OverloadedStrings #-}

module Type.Solve (infer) where

import Control.Monad.Reader
import Data.IORef as Ref

-- import Data.Map.Strict (Map)

import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Ast (Ast (..), Identifier, Literal (..))
import qualified Syntax.Ast as Ast
import Syntax.Infix (InfixOp (..))
import Syntax.TypeDef (TypeDefinition (..))
import Syntax.TypeRef (TypeRef (..))
import Type.Context (ContextM)
import qualified Type.Context as Ctx
import qualified Type.Error as Error
import Type.Type (Primitive (Lambda), Type (..))
import Prelude hiding (EQ, GT, LT)

eraseable = const True

inferLiteral = undefined

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

uniqueTypeVars :: [TypeRef] -> Set ByteString
uniqueTypeVars = foldMap go
 where
  go (VarT tv) = Set.singleton tv
  go (ApplicationT _ ts) = foldMap go ts
  go _ = Set.empty

insertTypeVars :: [(ByteString, Int)] -> [TypeRef] -> [Type]
insertTypeVars mappings = go
 where
  go (VarT tv : ts) = case lookup tv mappings of
    Just var -> TypeVar var : go ts
    Nothing -> error "This should be unreachable"
  -- NOTE: May need to change this from defined once we can have type sigs like:
  -- `return : a -> m a`
  go (ApplicationT t ts : xs) = TypeApplication (Defined t) (go ts) : go xs
  go _ = error "This should definitely be unreachable wtf"

infer :: Ast Type -> ContextM (Ast Type)
infer node = case node of
  Literal ty lit ->
    case ty of
      Unsolved -> inferLiteral lit
      _ -> return $ Literal ty lit
  Erase _ erasedBody -> do
    typed <- infer erasedBody
    if eraseable $ Ast.typeOf typed
      then
        return typed
      else do
        tell [Error.CannotErase $ Ast.typeOf typed]
        return typed
  Variable _ (Ast.Identifier varName) -> do
    scope <- asks Ctx.scope >>= liftIO . Ref.readIORef
    case Map.lookup varName scope of
      Just ty -> return $ Ast.withType ty node
      Nothing -> do
        varID <- Ctx.freshUVar
        let ty = UnificationVar varID
        Ctx.registerScope varName ty
        return $ Ast.withType ty node
  Infix _ op ->
    case Map.lookup op operatorTypes of
      Just signature -> undefined
      Nothing -> undefined
  _ -> undefined
