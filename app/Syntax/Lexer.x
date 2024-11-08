{

{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Syntax.Lexer(
  Alex
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

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Syntax.Token as T
import GHC.Generics hiding (to)

}

%wrapper "monadUserState-bytestring"

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


-- Regular Expressions

@newline = $return?$linefeed

@identifier = $lower [$lower$ident_legal$digit]*
@static = $upper [$lower$ident_legal$digit]*

$stringchar = [\0-\255] # [\"\\]   -- any byte except quote and backslash
@escape = \\\\ | \\\"               -- escaped backslash or quote
@string = \" ($stringchar | @escape)* \"
@char = \' ($stringchar | @escape) \'


tokens :- 
<0> @newline+ ;
<0> $horizontal_space+ ;
<0> "type" { to T.TYPE }
<0> "view" { to T.VIEW }
<0> "let" { to T.LET }
<0> "if" { to T.IF }
<0> "then" { to T.THEN }
<0> "else" { to T.ELSE }
<0> "match" { to T.MATCH }
<0> "with" { to T.WITH }
<0> "->" { to T.ARROW }
<0> "+" { to T.PLUS }
<0> "-" { to T.MINUS }
<0> "/" { to T.BACKSLASH }
<0> "*" { to T.ASTERIK }
<0> "%" { to T.PERCENT }
<0> "@" { to T.AT }
<0> ":" { to T.COLON }
<0> ";" { to T.SEMICOLON }
<0> "," { to T.COMMA }
<0> "==" { to T.ASSIGN }
<0> "=" { to T.EQ }
<0> "!=" { to T.NEQ }
<0> "<" { to T.LT }
<0> "<=" { to T.LTE }
<0> ">" { to T.GT }
<0> ">=" { to T.GTE }
<0> "!" { to T.EXCLAMATION }
<0> "(" { to T.RIGHT_PAREN }
<0> ")" { to T.LEFT_PAREN }
<0> "[" { to T.LEFT_SQUARE }
<0> "]" { to T.RIGHT_SQUARE }
<0> "{" { to T.LEFT_CURLY }
<0> "}" { to T.RIGHT_CURLY }
<0> $digit+ { tokInt }
<0> @string { tokString }
<0> @char { tokChar }
<0> @static { tokStatic }
<0> @identifier { tokId }


{

data AlexUserState 
  = AlexUserState

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex (Located T.Token)
alexEOF = do 
  (pos, _, _, _) <- alexGetInput
  pure $ Located T.EOI (Location pos pos)

data Location
  = Location {start :: AlexPosn, stop :: AlexPosn}
  deriving (Eq, Show, Ord)
data Located a
  = Located
  { inner :: !a
  , loc :: Location
  }
  deriving (Eq, Show, Ord, Generic, Functor, Foldable, Traversable)

newLoc :: AlexInput -> Int64 -> Location
newLoc (start, _, str, _) len =
  let stop = BS.foldl' alexMove start $ BS.take (fromIntegral len) str
   in Location{start, stop}

tokId :: AlexAction (Located T.Token)
tokId inp@(_, _, str, _) len =
  pure $ Located tok loc
 where 
  tok = T.Identifier $ BS.take (fromIntegral len) str
  loc = newLoc inp len

tokStatic :: AlexAction (Located T.Token)
tokStatic inp@(_, _, str, _) len = 
  pure $ Located tok loc
 where 
  tok = T.Static $ BS.take (fromIntegral len) str
  loc = newLoc inp len

tokInt :: AlexAction (Located T.Token)
tokInt inp@(_,_,str,_) len = 
  pure $ Located tok loc
 where
   tok = T.TInt $ read $ BS.unpack $ BS.take len str 
   loc = newLoc inp len 

tokString :: AlexAction (Located T.Token)
tokString inp@(_, _, str, _) len = 
  pure $ Located tok loc 
 where 
  tok = T.TString $ BS.take (fromIntegral (pred len)) (BS.drop 1 str)
  loc = newLoc inp len

tokChar :: AlexAction (Located T.Token)
tokChar inp@(_, _, str, _) len = 
  pure $ Located tok loc
 where 
  tok = T.TChar $ BS.head (BS.drop 1 str)
  loc = newLoc inp len

to :: T.Token -> AlexAction (Located T.Token)
to ctor inp len = pure $ Located ctor (newLoc inp len) 

scanMany :: ByteString -> Either String [Located T.Token]
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
unwrapTokens toks = go [] toks
  where 
    go acc (Located{inner}:xs) = go (inner:acc) xs
    go acc [] = reverse acc

}
