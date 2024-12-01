{-# LANGUAGE NamedFieldPuns #-}

module Syntax.Location (Located (..), Span, spans, same, withFile) where

import Data.ByteString.Lazy.Char8 (ByteString)

data Span = Span Int Int

data Located a
  = Located
  { start :: Span
  , end :: Span
  , file :: ByteString
  , inner :: a
  }

instance Semigroup (Located a) where
  (<>) l r = l{end = end r}

instance Functor Located where
  fmap f loc = loc{inner = f $ inner loc}

-- Maybe use non-empty?
spans :: [Located a] -> Located a
spans (x : xs) = foldl (<>) x xs
spans [] = error "Located.spans received empty list"

same :: a -> Located a -> Located a
same inner loc = loc{inner}

withFile :: ByteString -> Located a -> Located a
withFile file loc = loc{file}
