#!/usr/bin/env bash

# exit when any command fails
set -e

changes=$(git status --short nix/aspecs.json)
if [ -n "$changes" ]; then
    echo "Error: local changes in nix/aspecs.json"
    exit 1
fi

# update upstream reference
nix-shell -p nix-prefetch-scripts --run "nix-prefetch-git https://github.com/zoranbosnjak/asterix-specs.git > nix/aspecs.json"

# run all regenerate scripts
for i in $(find libs/ -type f | grep regenerate.sh); do
    cd $(dirname $i)
    ./regenerate.sh
    cd -
done;

