#!/bin/bash

if [ "$1" == "" ] || [ "$2" == "" ]; then
    echo "usage: rename_files_keep_suffix <pattern> <new-prefix>" >&2
    exit 1
fi

for filename in $1; do
    echo mv $filename "$2.${i##*.}";
    mv $filename "$2.${filename##*.}";
done
