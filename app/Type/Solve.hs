module Type.Solve (solve) where

import Control.Monad.Reader
import Syntax.Ast (Ast (..))
import Type.Context (ContextM)
import Type.Type (Type)

solve :: Ast Type -> ContextM [Ast Type]
solve node = do
  ctx <- ask
  case node of
    _ -> undefined
