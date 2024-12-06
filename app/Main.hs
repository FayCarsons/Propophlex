{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Cont (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Syntax.Lexer as Lexer
import Syntax.Parser (propophlex)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import Type.Context (ContextM, new, parse, runContext)

getTokens :: Text -> IO ()
getTokens input = case Lexer.scanMany input of
  Left e ->
    putStrLn "Failure: "
      *> pPrint e
  Right tokens -> do
    putStrLn "Success!\n"
    pPrint $ Lexer.unwrapTokens tokens

runParser :: Text -> ContextM ()
runParser input = do
  parse input propophlex >>= \case
    Left e ->
      liftIO $
        putStrLn "Failure: "
          *> pPrint e
    Right ast ->
      liftIO $
        putStrLn "Success!"
          *> pPrint ast

main :: IO ()
main = do
  input <- Text.readFile "app/phlib/main.phlex"
  getArgs
    >>= ( \case
            ["--tokens"] -> getTokens input
            ["-t"] -> getTokens input
            _ -> do
              ctx <- new
              (ast, _) <- runContext ctx (runParser input)
              print ast
        )
      . map (Text.toLower . Text.pack)
