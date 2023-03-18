#!/bin/bash
DUMP_PATH="$1"

if [ -z "$DUMP_PATH" ]; then
    echo "Missing dump path"
    exit 1
fi

# Cleanup old pngs
for png in $DUMP_PATH/*.png; do
    echo "Delete $png"
    rm $png
done

for dot_file in $DUMP_PATH/*.dot; do
    dot -Tpng -O $dot_file
done

xdg-open $DUMP_PATH/step1.dot.png