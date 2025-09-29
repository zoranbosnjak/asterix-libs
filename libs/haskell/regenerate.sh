#! /usr/bin/env nix-shell
#! nix-shell -i bash

# This script regenerates 'generated' files from the specs.
# It is normally called automatically as part of periodic updates.

set -e
export LC_ALL=C.UTF-8

# check local changes
changes1=$(git status --short .)
if [ -n "$changes1" ]; then
    echo "Error: local changes"
    exit 1
fi

# generated test specs
code-generator --language haskell \
    --test $TEST_ASTERIX_SPECS_FILES > test/Generated.hs

# generate actual specs
code-generator --language haskell \
    --ast-specs-ref $ASTERIX_SPECS_REF \
    --ast-specs-date $ASTERIX_SPECS_DATE \
    $ASTERIX_SPECS_FILES > src/Asterix/Generated.hs

# Bump version
changes2=$(git status --short .)
if [ -n "$changes2" ]; then
    echo "Bumping version"
    current_version=$(cat libasterix.cabal | grep "^version:" | sed 's/version:[ ]*//')
    IFS='.' read -r -a array <<< "$current_version"
    new_version="${array[0]}.$((array[1]+1)).0"
    sed -i -e "s/^version:.*/version:            $new_version/" libasterix.cabal
fi

