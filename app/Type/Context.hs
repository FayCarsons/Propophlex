{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Type.Context (Context (..), ContextM, new, lookupScope, lookupType, freshUVar, freshSkolem, registerType, registerScope, withNewScope) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.IORef (IORef)
import qualified Data.IORef as Ref
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Syntax.TypeDef (TypeDefinition)
import Type.Error (Error)
import qualified Type.Error as Error
import Type.Type (Type)

data Context
  = Context
  { scope :: IORef (Map Text Type)
  , types :: IORef (Map Text TypeDefinition)
  , nextUVar :: IORef Int
  , nextSkolem :: IORef Int
  }

newtype ContextM a
  = ContextM {runContextM :: ReaderT Context (WriterT [Error] IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Context
    , MonadWriter [Error]
    )

runContext :: Context -> ContextM a -> IO (a, [Error])
runContext ctx m = runWriterT $ flip runReaderT ctx $ runContextM m

new :: IO Context
new = do
  nextUVar <- Ref.newIORef 0
  nextSkolem <- Ref.newIORef 0
  scope <- Ref.newIORef Map.empty
  types <- Ref.newIORef Map.empty
  return Context{scope, types, nextUVar, nextSkolem}

modifyIORefM :: (MonadIO m) => IORef a -> (a -> (a, b)) -> m b
modifyIORefM ref f = liftIO $ Ref.atomicModifyIORef' ref f

lookupScope :: Text -> ContextM (Maybe Type)
lookupScope ident = do
  ctx <- ask
  result <- liftIO $ Map.lookup ident <$> Ref.readIORef (scope ctx)
  when (isNothing result) $
    tell [Error.UnboundVar ident]
  return result

lookupType :: Text -> ContextM (Maybe TypeDefinition)
lookupType typename = do
  ctx <- ask
  result <- liftIO $ Map.lookup typename <$> Ref.readIORef (types ctx)
  when (isNothing result) $
    tell [Error.UnknownType typename]
  return result

incr :: Int -> (Int, Int)
incr n = (succ n, n)

freshUVar :: ContextM Int
freshUVar = do
  ctx <- ask
  modifyIORefM (nextUVar ctx) incr

freshSkolem :: ContextM Int
freshSkolem = do
  ctx <- ask
  modifyIORefM (nextSkolem ctx) incr

registerType :: Text -> TypeDefinition -> ContextM ()
registerType typename typedef = do
  ctx <- ask
  liftIO $ Ref.modifyIORef' (types ctx) $ Map.insert typename typedef

registerScope :: Text -> Type -> ContextM ()
registerScope name ty = do
  ctx <- ask
  liftIO $ Ref.modifyIORef' (scope ctx) $ Map.insert name ty

clone :: IORef a -> IO (IORef a)
clone = Ref.readIORef >=> Ref.newIORef

withNewScope :: ContextM a -> ContextM a
withNewScope action = do
  ctx <- ask
  newCtx <- liftIO $ do
    scope' <- clone (scope ctx)
    return ctx{scope = scope'}
  local (const newCtx) action
