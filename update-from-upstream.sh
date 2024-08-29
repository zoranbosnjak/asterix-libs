#!/usr/bin/env bash

# update upstream reference
nix-prefetch-git https://github.com/zoranbosnjak/asterix-specs.git > nix/aspecs.json

# run all regenerate scripts
for i in $(find libs/ -type f | grep regenerate.sh); do
    cd $(dirname $i)
    ./regenerate.sh
    cd -
done;
