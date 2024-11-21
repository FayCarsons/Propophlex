## Code examples and brainstorming space
```haskell
-- Top level declarations
const Width : UInt32 = 512

-- Console here would be an algebraic effect
let println : String -> {Console} () = 
  fn s -> unsafe_print_string s

let print_with_tag : tag:String -> (ToString a) -> {Console} () 
  = (~tag: String, thing: ToString) -> println @ renamed_arg ++ ": " ++ thing;

-- Row polymorphic record type, a way to give a name to a view into a record
-- type
-- Single implicit required for 'a', Ref
type (Ref a) LabeledRef 
  = view { name : string, inner : a }

-- For multiple implicits we can do something like 
type a Foo = { name : String, inner : a } 
  with a: {ToString, Ref, Log}
  deriving (eq, show)

-- Declaring an implicit interface
protocol Log a is
  to_string : a -> String;
  typename : a -> String;

protocol Monad m is  
  bind : a T -> (a -> b T) -> b T; 
  pure : a -> a T;

protocol Ref a 
  alloc : Word -> {Alloc} a;
  drop : a -> {Dealloc} ();

type a Option = Some a | None
let bind : a Option -> (a -> b Option) -> b Option = 
  (Some a, f) -> f a;
  (None, _) -> None;

let pure : a -> a Option 
  = (a) -> Some a;

-- I think I'd like type declarations to lean on the OCaml side, as I generally
-- feel like it's the easiest language to casually define a type in, allowing
-- for things like declaring a sum type that's only returned from a single
-- function
type a Foo = Bar | Baz String | Quux (a Blim) | Goo { thing : a } 
  and a Blim = { name : String, value : a }

let double_signed : Int32 -> Int32 = fn[n] -> n * n

let print_with_name : a Named -> {console} () 
    = ({ name, inner }) -> print_with_tag ~tag:name (to_string inner);

-- Allow aliases for sets of trait bounds
let with_implicit = (fst : a, snd : b) 
  with a: { ToString, Log }, b: Ref ->
    drop snd;
    println @ to_string fst;
```
```
