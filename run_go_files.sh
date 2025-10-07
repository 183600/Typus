#!/bin/bash

# Script to run Go files in examples/go250923 directory
# Only displays error messages if they occur

for file in examples/go250923/*.go; do
    if [ -f "$file" ]; then
        echo "Testing $file..."
        error_output=$(cd examples && go run go250923/$(basename "$file") 2>&1 >/dev/null)
        if [ $? -ne 0 ]; then
            echo "Error in $file:"
            echo "$error_output"
            echo "---"
        else
            echo "$file: OK"
        fi
    fi
done