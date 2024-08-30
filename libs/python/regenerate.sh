#! /usr/bin/env nix-shell
#! nix-shell -i bash

set -e
export LC_ALL=C.UTF-8

changes1=$(git status --short .)
if [ -n "$changes1" ]; then
    echo "Error: local changes"
    exit 1
fi

# test specs
code-generator --language python \
    --test $TEST_ASTERIX_SPECS_FILES > tests/generated.py

# actual specs
code-generator --language python \
    --ast-specs-ref $ASTERIX_SPECS_REF \
    --ast-specs-date $ASTERIX_SPECS_DATE \
    $ASTERIX_SPECS_FILES > src/asterix/generated.py

# Bump version
changes2=$(git status --short .)
if [ -n "$changes2" ]; then
    echo "Bumping version"
    current_version=$(cat pyproject.toml | grep "version.*=" | sed 's/[^"]*//' | sed 's/\"//g')
    IFS='.' read -r -a array <<< "$current_version"
    new_version="${array[0]}.$((array[1]+1)).0"
    sed -i -e "s/version.*=.*/version = \"$new_version\"/g" pyproject.toml
fi
