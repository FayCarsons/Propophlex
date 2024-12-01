{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Infix (Fixity (..), InfixOp (..), operatorTable, operatorTokens, validateFixityMap, validateTokenMap) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Associativity (..))
import Language.Haskell.TH hiding (Fixity)
import Prelude hiding (EQ, GT, LT)

data Fixity
  = Fixity
  { precedence :: Int
  , associativity :: Associativity
  }
  deriving (Eq, Ord, Show)

data InfixOp
  = -- Arithmetic
    Plus
  | Minus
  | Mul
  | Div
  | GT
  | LT
  | GTE
  | LTE
  | EQ
  | NEQ
  | And
  | Or
  | Xor
  | -- combinators
    Bind
  | Pipe
  | Concat
  | Append
  | Cons
  | -- Bitwise
    BitAnd
  | BitOr
  | BitXor
  | LeftShift
  | RightShift
  | LeftRotate
  | RightRotate
  | -- Pointer ops
    Offset
  | Store
  deriving (Eq, Ord, Show, Read)

operatorTable :: Map InfixOp Fixity
operatorTable =
  Map.fromList
    [ -- Store/memory ops (lowest precedence)
      (Store, Fixity 1 RightAssociative)
    , (Offset, Fixity 2 LeftAssociative)
    , -- Pipe/bind/composition
      (Pipe, Fixity 3 LeftAssociative)
    , (Bind, Fixity 3 LeftAssociative)
    , -- List ops
      (Append, Fixity 4 RightAssociative)
    , (Concat, Fixity 4 RightAssociative)
    , (Cons, Fixity 4 RightAssociative)
    , -- Boolean ops
      (Or, Fixity 5 LeftAssociative)
    , (Xor, Fixity 6 LeftAssociative)
    , (And, Fixity 7 LeftAssociative)
    , -- Comparison
      (EQ, Fixity 8 LeftAssociative)
    , (NEQ, Fixity 8 LeftAssociative)
    , (GT, Fixity 8 LeftAssociative)
    , (LT, Fixity 8 LeftAssociative)
    , (GTE, Fixity 8 LeftAssociative)
    , (LTE, Fixity 8 LeftAssociative)
    , -- Bitwise ops
      (BitOr, Fixity 9 LeftAssociative)
    , (BitXor, Fixity 10 LeftAssociative)
    , (BitAnd, Fixity 11 LeftAssociative)
    , (LeftShift, Fixity 12 LeftAssociative)
    , (RightShift, Fixity 12 LeftAssociative)
    , (LeftRotate, Fixity 12 LeftAssociative)
    , (RightRotate, Fixity 12 LeftAssociative)
    , -- Arithmetic (highest)
      (Plus, Fixity 13 LeftAssociative)
    , (Minus, Fixity 13 LeftAssociative)
    , (Mul, Fixity 14 LeftAssociative)
    , (Div, Fixity 14 LeftAssociative)
    ]

operatorTokens :: Map Text InfixOp
operatorTokens =
  Map.fromList
    [ ("+", Plus)
    , ("-", Minus)
    , ("*", Mul)
    , ("/", Div)
    , (">", GT)
    , ("<", LT)
    , (">=", GTE)
    , ("<=", LTE)
    , ("==", EQ)
    , ("!=", NEQ)
    , ("&&", And)
    , ("||", Or)
    , ("^^", Xor)
    , (">>=", Bind)
    , ("|>", Pipe)
    , ("++", Concat)
    , ("<>", Append)
    , ("::", Cons)
    , ("&", BitAnd)
    , ("|", BitOr)
    , ("^", BitXor)
    , (">>", RightShift)
    , ("<<", LeftShift)
    , (">>>", RightRotate)
    , ("<<<", LeftRotate)
    , ("+>", Offset)
    , ("<<-", Store)
    ]

validateFixityMap :: Q [Dec]
validateFixityMap = do
  info <- reify ''InfixOp
  let constructors = case info of
        TyConI (DataD _ _ _ _ cons _) ->
          Set.fromList $ [nameBase name | (NormalC name _) <- cons]
        _ -> error "validateFixityMap: expected a data type"
      tableKeys = Set.fromList $ map show $ Map.keys operatorTable
      diff = Set.difference constructors tableKeys
  if not $ Set.null diff
    then fail $ "operatorTable does not handle operators: " ++ show (Set.toList diff)
    else return []

validateTokenMap :: Q [Dec]
validateTokenMap = do
  info <- reify ''InfixOp
  let constructors = case info of
        TyConI (DataD _ _ _ _ cons _) ->
          Set.fromList $ [nameBase name | (NormalC name _) <- cons]
        _ -> error "validateFixityMap: expected a data type"
      tableVals = Set.fromList $ map show $ Map.elems operatorTokens
      diff = Set.difference constructors tableVals
  if not $ Set.null diff
    then fail $ "operatorTokens does not handle operators: " ++ show (Set.toList diff)
    else return []
