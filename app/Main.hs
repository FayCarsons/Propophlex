module Main where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Syntax.Lexer as Lexer
import Syntax.Parser (propophlex)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

simple :: ByteString
simple =
  BS.pack $
    unlines $
      [ "type a Option = Some a | None"
      , "let map : (a -> b) -> a Option -> b Option = (o) -> 4 + 4;"
      ]

getTokens = case Lexer.scanMany simple of
  Left e -> do
    putStrLn "Failure: "
    pPrint e
  Right tokens -> do
    putStrLn "Success!\n"
    pPrint $ Lexer.unwrapTokens tokens

getAST = case Lexer.runAlex simple propophlex of
  Left e -> do
    putStrLn "Failure: "
    pPrint e
  Right ast -> do
    putStrLn "Success!"
    pPrint ast

main :: IO ()
main = getAST
