module Type.Solve (solve) where

import Syntax.Ast (Ast (..))
import Type.Context (ContextM)
import Type.Type (Type)

solve :: [Ast Type] -> ContextM [Ast Type]
solve = undefined
