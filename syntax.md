-- This is where I sketch out syntax

```rust
type a Option = Some a | None

// in Fn.phlex
const always : a -> (b -> a) = fn x -> (fn _ -> x)
const flip : (a -> b -> c) -> b -> a -> c = fn f a b -> f b a

const isSomeAnd : (a -> Bool) -> a Option -> Bool = fn f
  | Some x -> f x 
  | None -> False;

const isSome : a Option -> Bool = isSomeAnd (Fn.always True)

const sequence : a m List -> a List m with {m: Monad} = 
  fn xs -> 
    let go = fn acc 
      | x :: xs -> map (comp (flip go xs) (cons acc))

```
