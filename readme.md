# apply-unordered

This package is an experiment in type family / typeclass machinery to
provide type-directed function application and argument reordering. It
is severely restricted, in that it only supports monomorphic arguments
and parameters.  This allows you to do stuff like the following:

```haskell
import Control.Apply.Unordered

-- These will have the same value, "ccc", even though the arguments are
-- provided in a different order!

oneWay = replicateChar ? 'c' ? 3
otherWay = replicateChar ? 3 ? 'c'

replicateChar :: Int -> Char -> String
replicateChar = replicate
```

It also provides a function for type directed re-ordering of arguments:

```
flippedReplicateChar :: Char -> Int -> String
flippedReplicateChar = reorderArgs replicateChar
```

I considered naming / categorizing this as an `acme-` package, since
it's a mechanism that doesn't mesh well with Haskell's polymorphism.
However, I've decided that someone might actually find this useful or
interesting. Particularly:

* It allows you to pass arguments to functions without remembering the
  argument order. If the function's argument order is permuted, your
  code will still work.

* It probably usually optimizes away to 0 runtime overhead. I say
  "probably" because I have not benchmarked it or taken a look at the
  core. I did sprinkle `INLINE` pragmas everywhere, though.

* Incorrect usage often results in decent type errors, via custom type
  error messages.
