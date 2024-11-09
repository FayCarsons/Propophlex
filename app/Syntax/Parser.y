{
{-# LANGUAGE NamedFieldPuns #-}
module Syntax.Parser(propophlex) where

import Data.Monoid (First(..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LB
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Syntax.Token as T
import qualified Syntax.Lexer as Lexer 
import Syntax.Lexer (Located(..))
import qualified Syntax.Ast as Ast
}

%name propophlex
%tokentype { Located T.Token }
%error { parseError }
%monad { Lexer.Alex } { >>= } { pure }
%lexer { lexer } { Located T.EOI _ }
%token 
  '(' { Located T.LEFT_PAREN _ }
  ')' { Located T.RIGHT_PAREN _ }
  '{' { Located T.LEFT_CURLY _ }
  '}' { Located T.RIGHT_CURLY _ }
  '[' { Located T.LEFT_SQUARE _ }
  ']' { Located T.RIGHT_SQUARE _ }
  EOI { Located T.EOI _ }
  '+' { Located T.PLUS _ }
  '-' { Located T.MINUS _ }
  '/' { Located T.BACKSLASH _ }
  '*' { Located T.ASTERIK _ }
  '%' { Located T.PERCENT _ }
  '@' { Located T.AT _ }
  ':' { Located T.COLON _ }
  ';' { Located T.SEMICOLON _ }
  ',' { Located T.COMMA _ }
  '=' { Located T.ASSIGN _ }
  eq  { Located T.EQ _ }
  neq { Located T.NEQ _ }
  lt  { Located T.LT _ }
  lte { Located T.LTE _ }
  gt  { Located T.GT _ }
  gte { Located T.GTE _ }
  '!'  { Located T.EXCLAMATION _ }
  arrow { Located T.ARROW _ }
  int { Located (T.TInt $$) _ }
  float { Located (T.TFloat $$) _ }
  char { Located (T.TChar $$) _ }
  string { Located (T.TString $$) _ }
  identifier { Located (T.Identifier $$) _ }
  static { Located (T.Static $$) _ }
  type { Located T.TYPE _ }
  view { Located T.VIEW _ }
  let { Located T.LET _ }
  if { Located T.IF _ }
  then { Located T.THEN _ }
  else { Located T.ELSE _ }
  match { Located T.MATCH _ }
  with { Located T.WITH _ }

%left '+' '-'
%left '*' '/'
%nonassoc NEG

%%

Program : Expressions { reverse $1 }

Expressions : Expression { [$1] }
           | Expressions Expression { $2 : $1 }

Expression : Literal { $1 }
          | identifier { Ast.variable $1 }
          | '(' Expression ')' { $2 }
          | Expression '+' Expression { Ast.BinaryOp Ast.Add $1 $3 }
          | Expression '-' Expression { Ast.BinaryOp Ast.Sub $1 $3 }
          | Expression '*' Expression { Ast.BinaryOp Ast.Mul $1 $3 }
          | Expression '/' Expression { Ast.BinaryOp Ast.Div $1 $3 }
          | '-' Expression %prec NEG { Ast.UnaryOp Ast.Neg $2 }
          | let identifier ':' Signature '=' Expressions ';' { Ast.letDeclaration $2 (Just $4) $ reverse $6 }
          | let identifier '=' Expressions ';' { Ast.letDeclaration $2 Nothing $4 }
          | Lambda { $1 }

TypeVars : identifier { [ $1 ] }
         | TypeVars identifier { $2 : $1 }

TypeRef : static { Ast.typeConcrete $1 }
        | static TypeVars { Ast.typeApplication $1 $ reverse $2 }
        | identifier { Ast.typeVar $1 }

Arrows : TypeRef arrow TypeRef { [$1, $3] }
       | Arrows arrow TypeRef { $3 : $1 }

Signature : TypeRef { Ast.LiteralT $1 }
          | Arrows { Ast.FnT $ reverse $1 }

Arg : identifier { Ast.untypedArg $1 }
    | identifier ':' TypeRef { Ast.typedArg $1 $3 }

Args : Arg { [ $1 ] }
     | Args ',' Arg { $3 : $1 }

Lambda : '(' Args ')' arrow Expressions { Ast.Lambda (reverse $2) (reverse $5) }

Literal : int { Ast.int $1 }
        | float { Ast.float $1 }
        | char { Ast.char $1 }
        | string { Ast.string $1 }

{

slice offset len = take len . drop offset
parseError :: Located T.Token -> Lexer.Alex a
parseError Located{inner, loc} = do 
  (Lexer.AlexPn offset line column, prevChar, pendingInput, _) <- Lexer.alexGetInput
  let token = inner
      contextLines = 2
      input = pendingInput
      allLines = Text.splitOn (Text.pack "\n") $ Text.pack (BS.unpack input)
      startLine = max 1 (line - contextLines)
      endLine = min (length allLines) (line + contextLines)
      surrounding = zip [startLine..] (slice (pred startLine) (succ endLine - startLine) allLines)
      pointer = replicate (pred column) ' ' <> "^"
      formatLine (ln, content) =
        show ln <> " | " <> Text.unpack content <>
        (if ln == line then 
            "\n" <> (replicate (length (show ln) + 3) ' ') <> pointer
          else "")

      errorMsg = unlines $  
        [ "Parse error at line " <> show line <> ", column " <> show column
        , "Unexpected token: " <> show token
        , "Context:" 
        ] ++ map formatLine surrounding

  Lexer.alexError errorMsg

lexer :: (Located T.Token -> Lexer.Alex a) -> Lexer.Alex a
lexer = (=<< Lexer.alexMonadScan)
}
