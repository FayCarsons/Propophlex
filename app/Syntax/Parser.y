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
  ':' { Located T.COLON _ }
  ';' { Located T.SEMICOLON _ }
  ',' { Located T.COMMA _ }
  '=' { Located T.ASSIGN _ }
  '!'  { Located T.EXCLAMATION _ }
  '|' { Located T.BAR _ }
  '.' { Located T.PERIOD _ }
  '_' { Located T.UNDERSCORE _ }
  binop { Located (T.TBinop $$) _ }
  arrow { Located T.ARROW _ }
  int { Located (T.TInt $$) _ }
  float { Located (T.TFloat $$) _ }
  char { Located (T.TChar $$) _ }
  string { Located (T.TString $$) _ }
  identifier { Located (T.Identifier $$) _ }
  constIdent { Located (T.ConstIdent $$) _ }
  type { Located T.TYPE _ }
  view { Located T.VIEW _ }
  let { Located T.LET _ }
  const { Located T.CONST _ }
  if { Located T.IF _ }
  then { Located T.THEN _ }
  else { Located T.ELSE _ }
  match { Located T.MATCH _ }
  with { Located T.WITH _ }
  fn { Located T.FN _ }

%left ';'
%left APPLICATION
 
%%

Program : TopLevels { reverse $1 }

TopLevels : TopLevel { [$1] }
          | TopLevels TopLevel { $2 : $1 }

TopLevel : TypeDeclaration { $1 }
         | ConstDeclaration { $1 }

ConstDeclaration : const identifier ':' Signature '=' Expression { Ast.constDeclaration $2 $4 $6 } 

Expressions : Expression { [$1] }
            | Expressions Expression { $2 : $1 }

Expression : Literal { Ast.literal $1 }
           | Arithmetic { $1 }
           | SimpleExpr { $1 }
           | Call { $1 } 
           | LetBinding { $1 }
           | Lambda { $1 }
           | If { $1 }
           | Match { $1 }

Call : Call SimpleExpr %prec APPLICATION { Ast.call $1 $2 }
     | SimpleExpr SimpleExpr %prec APPLICATION { Ast.call $1 $2 }

If : if Expression then Expression { Ast.ifThen $2 $4 }
   | if Expression then Expression else Expression { Ast.ifThenElse $2 $4 $6 }

MatchArm : '|' Literal arrow Expression { (Ast.matchLiteral $2, $4) }
         | '|' identifier arrow Expression { (Ast.matchBinding $2, $4) }
         | '|' '_' arrow Expression { (Ast.matchErase, $4) }

MatchArms : MatchArm { [$1] }
          | MatchArms MatchArm { $2 : $1 }

Match : match Expression MatchArms { Ast.match $2 $ reverse $3 }

SimpleExpr : identifier { Ast.variable $1 }
           | FieldAccess { $1 }
           | '(' Expression ')' { $2 }

LetBinding : let identifier ':' Signature '=' Expressions ';' { Ast.letDeclaration $2 (Just $4) $ reverse $6 }
           | let identifier '=' Expressions ';' { Ast.letDeclaration $2 Nothing $4 }
           | let '_' '=' Expression ';' { Ast.erase $4 }

Arithmetic : Expression binop Expression { Ast.binaryInfix $2 $1 $3 }
           | binop Expression { Ast.unary $1 $2 }

TypeArg : identifier { Ast.typeVar $1 }
        | constIdent { Ast.typeConcrete $1 }
        | '(' TypeArgs ')' { Ast.typeApplication $2 }

TypeArgs : TypeArg TypeArg { [$2, $1] }
         | TypeArgs TypeArg { $2 : $1 }

TypeRef : TypeArg { $1 }
        | TypeArgs { Ast.typeApplication $1 }
        | '(' ')' { Ast.unitT }
        | '(' TypeRef arrow TypeRef ')' { Ast.fnType [$2, $4] }  -- Directly construct function type
        | '(' Arrows ')' { Ast.fnType (reverse $2) }  -- Handle multi-arg functions

Arrows : TypeRef arrow TypeRef { [$1, $3] }
       | Arrows arrow TypeRef { $3 : $1 }

Signature : TypeRef { $1 }
          | Arrows { Ast.fnType $1 }

Arg : identifier { Ast.untypedArg $1 }
    | identifier ':' TypeRef { Ast.typedArg $1 $3 }
    | '_' { Ast.erasedArg }

Args : Arg { [ $1 ] }
     | Args Arg { $2 : $1 }
     | RecordDestructure { $1 }
     | RecordDestructure Arg {  $2 : $1 }

Lambda : fn Args arrow Expressions { Ast.lambda (reverse $2) (reverse $4) }
       | fn MatchArms { Ast.lambdaMatch $ reverse $2 }

Literal : int { Ast.int $1 }
        | float { Ast.float $1 }
        | char { Ast.char $1 }
        | string { Ast.string $1 }
        | '(' TupleFields ')' { Ast.tuple (reverse $2) }
        | '(' ')' { Ast.unitLiteral }
        | RecordLiteral { $1 }
        | SumLiteral { $1 }

TupleFields : Literal { [$1] }
            | TupleFields ',' Literal { $3 : $1 }
 
SumField : constIdent TypeRef { ($1, $2) }
         | constIdent '{' RecordFields '}' { ($1, Ast.anonymousRecordT $ reverse $3) }
         | constIdent { ($1, Ast.unitT) }

SumFields : SumField { [$1] }
          | SumFields '|' SumField { $3 : $1 }

SumLiteral : constIdent Expressions { Ast.sumLiteral $1 (reverse $2) }
           | constIdent { Ast.sumLiteral $1 [] }

RecordField : identifier ':' TypeRef { ($1, $3) }
RecordFields : RecordField { [$1] }
             | RecordFields ',' RecordField { $3 : $1 }

RecordLiteralField : identifier '=' Expression { ($1, $3) } 
RecordLiteralFields : RecordLiteralField { [$1] }
                    | RecordLiteralFields ',' RecordLiteralField { $3 : $1 }
RecordLiteral : '{' RecordLiteralFields '}' { Ast.recordLiteral $ reverse $2 }

RecordFieldPun : identifier { [ Ast.untypedArg $1] }
               | '_' { [ Ast.erasedArg ] }
               | RecordFieldPun ',' identifier { Ast.untypedArg $3 : $1 }
               | RecordFieldPun ',' '_' { Ast.erasedArg : $1 }

RecordDestructure : '{' RecordFieldPun '}' { $2 }

FieldAccess : identifier '.' identifier { Ast.fieldAccess $1 $3 }

TypeDeclaration : type constIdent '=' SumFields { Ast.sumTypeDeclaration [Ast.typeConcrete $2] (reverse $4) }
                | type TypeArgs '=' SumFields { Ast.sumTypeDeclaration $2 (reverse $4) }
                | type constIdent '=' '{' RecordFields '}' { Ast.recordTypeDeclaration [Ast.typeConcrete $2] (reverse $5) }
                | type TypeArgs '=' '{' RecordFields '}' { Ast.recordTypeDeclaration (reverse $2) (reverse $5) }

{

slice :: Int -> Int -> [a] -> [a]
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
