module Main where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Syntax.Lexer as Lexer
import Syntax.Parser (propophlex)
import Text.Pretty.Simple (pPrint)
import Type.Context (new, runT)

getTokens :: Text -> IO ()
getTokens input = case Lexer.scanMany input of
  Left e ->
    putStrLn "Failure: "
      *> pPrint e
  Right tokens -> do
    putStrLn "Success!\n"
    pPrint $ Lexer.unwrapTokens tokens

getAST :: Text -> IO ()
getAST input = do
  context <- new
  case Lexer.runAlex input (runT context propophlex) of
    Left e ->
      putStrLn "Failure: "
        *> pPrint e
    Right ast ->
      putStrLn "Success!"
        *> pPrint ast

main :: IO ()
main =
  Text.readFile "app/phlib/main.phlex"
    >>= getAST
