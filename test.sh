#!/bin/bash

input="$1"

files=`find ctest -name '*.c' | sed -e s/.c$//`

for file in $files; do
    cargo run --quiet "$file.c" > "$file.s"
    if [ $? -ne 0 ]; then
        cat "$file.s"
        exit 1
    fi

    gcc -o "$file.o" -c "$file.s"
done

echo "$files" | sed -e s/$/.o/ | xargs gcc -o ctest/test 
./ctest/test
echo $?
