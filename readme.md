# apply-unordered-mono

This package is an experiment in type family / typeclass machinery to
provide type-directed function application and argument reordering. It
is severely restricted, in that it only supports monomorphic arguments
and parameters.  This allows you to do stuff like the following:

```haskell
import Control.Apply.Unordered.Mono ((?), reorderArgs)

-- These will have the same value, "ccc", even though the arguments are
-- provided in a different order!

oneWay = replicateChar ? 'c' ? 3
otherWay = replicateChar ? 3 ? 'c'

replicateChar :: Int -> Char -> String
replicateChar = replicate
```

It also provides a function for type directed re-ordering of arguments:

```
f1 :: A -> B -> C -> D

f2 :: C -> A -> B -> D
f2 = reorderArgs f1

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

# apply-unordered

I wrote this package after `apply-unordered`. The goal is to do this
unordered application, but in a way that is compatible with Haskell's
polymorphism. Here's how it works:

* `Control.Apply.Positional` provides an `applyAt` function which lets
  you write `(applyAt @1 replicateChar 'c') 3`. The `@1` specifies the
  function parameter index to provide the argument to.

* `Control.Apply.Unordered.Plugin` provides a GHC plugin that replaces
  occurrences of the type family `BestParamIxImpl (a :: *) (f :: *)`
  with a type level natural indicating the index of the parameter of
  `f` that best matches the argument type `a`. This "best matching"
  logic works exactly the same as matching the type parameters of
  overlapping instances.

* `Control.Apply.Unordered` sticks `BestParamIxImpl` together with the
  `applyAt` machinery to yield a better `?` function that can handle
  polymorphic functions!

This already seems to work well, but the code could use some more
polish and testing.

[magic-tyfams]: https://github.com/isovector/type-sets/tree/master/magic-tyfams
