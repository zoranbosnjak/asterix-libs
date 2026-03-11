#!/usr/bin/env bash

if [ $# -eq 0 ]; then
    samples=$(ls readme-samples/*py)
else
    samples=$(ls $@)
fi

# run all examples
for i in $samples; do
    echo "---"
    echo "running: $i"
    python $i
    if [ $? -ne 0 ]; then
        echo ""
        echo "Failure in $i!"
        echo ""
        exit 1
    fi
done
echo ""
echo "All done. OK."

