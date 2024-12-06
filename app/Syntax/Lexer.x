{
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax.Lexer(
  Alex
  , AlexUserState
  , AlexInput
  , AlexPosn(..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  , Location(..)
  , Located(..)
  , scanMany
  , unwrapTokens
) where

import Data.Text (Text)
import Debug.Trace (trace)
import qualified Data.Text as Text
import Data.Int (Int64)
import qualified Syntax.Token as T
import qualified Data.Map as Map
import Syntax.Infix (InfixOp(..), operatorTokens)
import GHC.Generics hiding (to)

}

%wrapper "monadUserState-strict-text"

-- Character sets

$digit = [0-9]
$hexdigit     = [0-9a-fA-F]
$lower        = [a-z]
$upper        = [A-Z]
$letter       = [$lower$upper]
$space        = [\ ]
$tab          = [\t]
$return       = \r
$linefeed     = \n
$ascii      = [\x21-\x7E]
$cont         = [\x80-\xBF]
$ident_legal = [\'\_\?$digit]
$horizontal_space = [$space$tab]
$operator_char = [\+\-\*\/\>\<\=\|\&\^\!\@\#\$\%\~\:\.]

-- Regular Expressions

@newline = $return?$linefeed

@identifier = $lower [$letter$ident_legal$digit]*
@const = $upper [$letter$ident_legal$digit]*

$stringchar = [\0-\255] # [\"\\]   -- any byte except quote and backslash
@escape = \\\\ | \\\"               -- escaped backslash or quote
@string = \" ($stringchar | @escape)* \"
@char = \' ($stringchar | @escape) \'


tokens :- 
  @newline+ ;
  $horizontal_space+ ;
  "type" { to T.TYPE }
  "view" { to T.VIEW }
  "let" { to T.LET }
  "const" { to T.CONST }
  "extern" { to T.EXTERN }
  "if" { to T.IF }
  "then" { to T.THEN }
  "else" { to T.ELSE }
  "match" { to T.MATCH }
  "with" { to T.WITH }
  "fn" { to T.FN }
  "->" { to T.ARROW }
  "/" { to T.BACKSLASH }
  ":" { to T.COLON }
  ";" { to T.SEMICOLON }
  "," { to T.COMMA }
  "=" { to T.ASSIGN }
  "!" { to T.EXCLAMATION }
  "|" { to T.BAR }
  "." { to T.PERIOD }
  "_" { to T.UNDERSCORE }
  "(" { to T.LEFT_PAREN }
  ")" { to T.RIGHT_PAREN }
  "[" { to T.LEFT_SQUARE }
  "]" { to T.RIGHT_SQUARE }
  "{" { to T.LEFT_CURLY }
  "}" { to T.RIGHT_CURLY }
  $operator_char+ { matchOperator }
  $digit+ { tokInt }
  @string { tokString }
  @char { tokChar }
  @const { tokStatic }
  @identifier { tokId }


{

data AlexUserState = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex (Located T.Token)
alexEOF = do 
  (pos, _, _, _) <- alexGetInput
  pure $ Located T.EOI (Location pos pos)

data Location = Location 
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show, Ord)

data Located a = Located
  { inner :: !a
  , loc :: Location
  } deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

newLoc :: AlexInput -> Int -> Location
newLoc (start, _, _, str) len =
  let stop = Text.foldl' alexMove start (Text.take len str)
   in Location{start, stop}

matchOperator :: AlexAction (Located T.Token)
matchOperator inp@(_, _, _, str) len = trace ("Got operator: " ++ Text.unpack str) $ case Map.lookup (Text.take len $ Text.strip str) operatorTokens of 
    Just binop -> to (T.TBinop binop) inp len 
    Nothing -> alexError $ "Unknown operator: " ++ Text.unpack str

tokId :: AlexAction (Located T.Token)
tokId inp@(_, _, _, str) len =
  pure $ Located tok loc
 where 
  tok = T.Identifier $ Text.take len str
  loc = newLoc inp len

tokStatic :: AlexAction (Located T.Token)
tokStatic inp@(_, _, _, str) len = 
  pure $ Located tok loc
 where 
  tok = T.ConstIdent $ Text.take len str
  loc = newLoc inp len

tokInt :: AlexAction (Located T.Token)
tokInt inp@(_, _, _, str) len = 
  pure $ Located tok loc
 where
   tok = T.TInt $ read $ Text.unpack $ Text.take len str 
   loc = newLoc inp len 

tokString :: AlexAction (Located T.Token)
tokString inp@(_, _, _, str) len = 
  pure $ Located tok loc 
 where 
  tok = T.TString $ Text.take (pred len) (Text.drop 1 str)
  loc = newLoc inp len

tokChar :: AlexAction (Located T.Token)
tokChar inp@(_, _, _, str) len = 
  pure $ Located tok loc
 where 
  tok = T.TChar $ Text.head (Text.drop 1 str)
  loc = newLoc inp len

to :: T.Token -> AlexAction (Located T.Token)
to ctor inp len = pure $ Located ctor $ newLoc inp len

scanMany :: Text -> Either String [Located T.Token]
scanMany input = runAlex input go
  where 
    go = do 
      output <- alexMonadScan
      if inner output == T.EOI 
        then 
          pure [output]
        else 
          (output :) <$> go

unwrapTokens :: [Located a] -> [a]
unwrapTokens = map inner
}
