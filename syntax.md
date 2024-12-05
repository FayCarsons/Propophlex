-- This is where I sketch out syntax

```haskell
main = 
  let 
    x = 420
    y = 69
    z = "hewwo"
  in
  (x * y)

type t n Pairs = { idx : Word, next = (T, T) Option }

const pairs : <T, N> [T, N] -> T N Pairs 
  where N: Const<Word> = 
    fn arr -> 
      { idx = 0, next = None }
```
