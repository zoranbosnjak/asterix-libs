haskell:
- when constructing, only 'Builder' is required, not a complete structure
  (no need to inspect further)

python:
- return instead of raise
  It's more general, on the calling site, it can be turned (with a decorator)
  into exception or some Maybe monad or...

