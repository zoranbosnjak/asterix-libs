haskell:
- when constructing, only 'Builder' is required, not a complete structure
  (no need to inspect further)
- try to use ST trick when parsing, such that only need to remember offset
  and size of the individual item, but not the ByteString (Bits) itself.

python:
- return instead of raise
  It's more general, on the calling site, it can be turned (with a decorator)
  into exception or some Maybe monad or...

