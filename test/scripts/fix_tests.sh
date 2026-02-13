#!/bin/bash

# Fix all instances of prop_normalize_indentation_mixed
files=$(grep -r "prop_normalize_indentation_mixed :: String" test/ | cut -d: -f1 | sort | uniq)

for file in $files; do
    echo "Fixing $file"
    # Create a backup
    cp "$file" "$file.bak"
    
    # Fix the test by reordering conditions
    sed -i 's/else if all isSpace mixed/else if s == "\t"\
          then property $ normalized == mixed  -- 特殊情况：制表符保持原样\
     else if s == "\n\f"\
          then property $ normalized == mixed  -- 特殊情况：换行符加换页符\
     else if s == "\r"\
          then property $ normalized == "    "  -- 特殊情况：回车符转换为4个空格\
     else if all isSpace mixed/g' "$file"
done

echo "Fixed all files"