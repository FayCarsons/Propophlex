module Main where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Syntax.Lexer as Lexer
import Syntax.Parser (propophlex)
import Text.Pretty.Simple (pPrint)

getTokens :: ByteString -> IO ()
getTokens input = case Lexer.scanMany input of
  Left e -> do
    putStrLn "Failure: "
    pPrint e
  Right tokens -> do
    putStrLn "Success!\n"
    pPrint $ Lexer.unwrapTokens tokens

getAST :: ByteString -> IO ()
getAST input = case Lexer.runAlex input propophlex of
  Left e -> do
    putStrLn "Failure: "
    pPrint e
  Right ast -> do
    putStrLn "Success!"
    pPrint ast

main :: IO ()
main = BS.readFile "app/main.phlex" >>= getAST
