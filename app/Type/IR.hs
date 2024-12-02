{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Type.IR () where

import Data.Functor ((<&>))
import Data.Text (Text)
import Syntax.Ast (Ast)
import qualified Syntax.Ast as Ast
import Syntax.TypeRef (TypeRef)
import Type.Context (ContextM)
import qualified Type.Context as Ctx

{-
data Literal a
  = Int Int
  | Float Double
  | Char Char
  | Bool Bool
  | Tuple Int [Node a]
  | Record [(Text, Node a)]
  | Sum Text [Node a]
  | Unit

data Expr annotation
  = Literal (Literal annotation)
  | Call (Node annotation) [Node annotation]
  | Def Text (Maybe TypeRef) [Node annotation]
  | Do (Maybe TypeRef) (Node annotation)

data Identifier
  = Ident Text
  | FieldAccess Text Text

data Node annotation
  = Identifier annotation Identifier
  | Expr annotation (Expr annotation)

refineLiteral :: Ast.Literal a -> ContextM (Literal a)
refineLiteral = \case
  Ast.LInt n -> return $ Int n
  Ast.LFloat n -> return $ Float n
  Ast.LChar c -> return $ Char c
  Ast.LBool b -> return $ Bool b
  Ast.LTuple t -> Tuple (length t) <$> mapM refine t
  Ast.LSum (Ast.Const variant) fields -> mapM refine fields <&> Sum variant
  Ast.LRecord fields -> Record <$> mapM (\(Ast.Identifier f, v) -> refine v <&> (f,)) fields

refine :: Ast a -> ContextM (Node a)
refine =
  \case
    Ast.Literal t l -> Expr t . Literal <$> refineLiteral l
    Ast.Erase t sig body -> Expr t . Do sig <$> refine body
    Ast.Let t (Ast.Identifier name) sig body -> Expr t . Def name sig <$> mapM refine body
    Ast.Constant t (Ast.Const name) sig body -> Expr t . Def name (Just sig) . (: []) <$> refine body
    Ast.Extern t (Ast.Identifier name) sig symbol -> undefined
    Ast.Call t f x -> undefined
 - -}
