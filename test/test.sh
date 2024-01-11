#!/bin/bash

# If number of arguments less then 1; print usage and exit
if [ $# -lt 1 ]; then
    printf "Usage: %s <application>\n" "$0" >&2
    exit 1
fi

bin="$1"           # The application (from command arg)
diff="diff -iaBEdu --color"   # Diff command, or what ever

mkdir -p test/out

# Loop the array
for file_in in test/*.v; do
    # Padd file_base with suffixes
    file_name="${file_in##*/}"
    file_basename="${file_name%.*}"
    file_out_val="test/$file_basename.out"       # The out file to check against
    file_out_tst="test/out/$file_basename.out.tst"   # The outfile from test application
    file_out_asm="test/out/$file_basename.s"   # The outfile from test application
    file_out_exe="test/out/$file_basename.exe"   # The outfile from test application

    if [ ! -f "$file_out_val" ]; then
        printf "\x1b[31m[  FAILED  ]\x1b[0m $file_in: validation file $file_out_val is missing\n"
        continue;
    fi

    # Run application, redirect in file to app, and output to out file
    $bin "$file_in" > "$file_out_asm"
    if [ $? != 0 ]; then
      printf "\x1b[31m[  FAILED  ]\x1b[0m $file_in: failed to compile\n"
      continue
    fi

    gcc -g "$file_out_asm" -o "$file_out_exe"
    "./$file_out_exe" > "$file_out_tst"

    # Execute diff
    $diff "$file_out_tst" "$file_out_val"
    if [ $? != 0 ]; then
      printf "\x1b[31m[  FAILED  ]\x1b[0m $file_in: diff failed\n"
      continue
    else
      printf "\x1b[32m[  PASSED  ]\x1b[0m $file_in\n"
    fi
done

# Clean exit with status 0
exit 0
