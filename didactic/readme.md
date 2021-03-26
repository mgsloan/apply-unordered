These files can be evaluated via `stack runghc FILE` or `stack ghci
FILE`. They are intended to accompany a blog post or two explaining
how apply-unordered works.

* `v1.hs` version attempting to use overlapping associated type
  families (doesn't work).

* `v2.hs` version attempting to use closed type family for result type
  (doesn't work).

* `v3.hs` version which works by using a closed type family to select
  instances.

* `v4.hs` version which uses a custom type error message, but the type
  error is in terms of the result type of the function instead of the
  full function type.

* `v5.hs` version improves the custom error message via a type family
  that recurses on the function type, but also passes the full
  function type for reporting.
