module Main where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Syntax.Lexer as Lexer
import Syntax.Parser (propophlex)
import System.Environment (getArgs)

simple :: ByteString
simple = BS.pack "let sq : Int -> Int = (n) -> n * n; let starting_value = 4; let result = sq starting_value;"

main :: IO ()
main = do
  args <- getArgs
  case args of
    "impossible" : _ -> putStrLn $ case Lexer.scanMany simple of
      Left e -> "Lexing failure: " ++ show e
      Right tokens -> "Success! Tokens:\n" ++ show (Lexer.unwrapTokens tokens)
    _ -> putStrLn $ case Lexer.runAlex simple propophlex of
      Left e -> show e
      Right ast -> "Success! AST:\n" ++ show ast
