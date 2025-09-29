# Implementation remarks

## Dependent string

A scenario where the 'string' content is a one of the cases in a dependent
content. This scenario is not actually defined in real specs and is
currently not covered in test specs. The problem is that the implementation
handles dependent quantity and dependent sctring separately.

To solve the problem, either:
    - unify (generalize) dependent implementation (and keep test
      specs as is)
    - or add dependent string to the test specs

## Code repetition

Check if it's possible to unifiy some patterns, which are currently
implemented with a lot of code repetition:

- `class MkExtended` and `class MkExtendedGroups`... and the instances
  look almost exactly the same.

## getVariation

Check if possible not to use `getVariation` explicitely.

