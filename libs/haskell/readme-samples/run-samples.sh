#!/usr/bin/env bash

if [ $# -eq 0 ]; then
    samples=$(ls readme-samples/*hs)
else
    samples=$(ls $@)
fi

ghcOpts="-Wall \
-Wincomplete-uni-patterns \
-Wincomplete-record-updates \
-Wcompat \
-Widentities \
-Wredundant-constraints \
-Wunused-packages \
-Wpartial-fields"

# run all examples
for i in $samples; do
    echo "---"
    echo "running: $i"
    nix-shell --command "runhaskell $ghcOpts -isrc $i"
    if [ $? -ne 0 ]; then
        echo ""
        echo "Failure in $i!"
        echo ""
        exit 1
    fi
done
echo ""
echo "All done. OK."

