module Main where
import qualified Syntax.Lexer as Lexer
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

simple :: ByteString
simple = BS.pack "let c : Char = \'c\'"

main :: IO ()
main = 
  putStrLn $ case Lexer.scanMany simple of 
    Left errs -> "Failed: " ++ (show errs)
    Right tokens -> "Tokens:\n" ++ (show $ Lexer.unwrapTokens tokens)
