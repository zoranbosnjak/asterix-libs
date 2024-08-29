#! /usr/bin/env nix-shell
#! nix-shell -i bash

export LC_ALL=C.UTF-8

# test specs
code-generator --language python \
    --test $TEST_ASTERIX_SPECS_FILES > tests/generated.py

# actual specs
code-generator --language python \
    --ast-specs-ref $ASTERIX_SPECS_REF \
    --ast-specs-date $ASTERIX_SPECS_DATE \
    $ASTERIX_SPECS_FILES > src/asterix/generated.py
