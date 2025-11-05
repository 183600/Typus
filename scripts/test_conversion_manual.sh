#!/bin/bash

echo "=== Manual Typus Conversion Test ==="

# Test configuration
input_dir="examples/go250923"
output_dir="examples/go250923output"

# Step 1: Test typus convert command
echo "Step 1: Testing typus convert command..."
if stack exec -- typus convert "$input_dir" -o "$output_dir"; then
    echo "✓ typus convert command completed successfully"
else
    echo "ERROR: typus convert command failed"
    exit 1
fi

echo ""

# Step 2: Get list of typus files
echo "Step 2: Getting list of typus files..."
typus_files=($input_dir/*.typus)
echo "Found ${#typus_files[@]} typus files"

echo ""

# Step 3: Test running original typus files (expected to fail)
echo "Step 3: Testing original typus files (expected to fail)..."
for file in "${typus_files[@]}"; do
    echo "Testing original file: $(basename "$file")"
    if go run "$file" 2>/dev/null; then
        echo "WARNING: Original typus file executed successfully: $file"
    else
        echo "✓ Expected failure (Go cannot run .typus files directly)"
    fi
done

echo ""

# Step 4: Test running converted Go files
echo "Step 4: Testing converted Go files..."
success_count=0
total_count=0

for typus_file in "${typus_files[@]}"; do
    base_name=$(basename "$typus_file" .typus)
    go_file="$output_dir/${base_name}.go"

    if [ -f "$go_file" ]; then
        echo "Testing Go file: $(basename "$go_file")"
        total_count=$((total_count + 1))

        if timeout 10s go run "$go_file" >/dev/null 2>&1; then
            echo "✓ Go file executed successfully"
            success_count=$((success_count + 1))
        else
            echo "✗ Go file execution failed"
        fi
    else
        echo "ERROR: Go file not found: $go_file"
    fi
done

echo ""

# Step 5: Summary
echo "=== Test Summary ==="
echo "Total typus files: ${#typus_files[@]}"
echo "Go files tested: $total_count"
echo "Go files that run without error: $success_count"

if [ $success_count -eq $total_count ]; then
    echo "✓ All tests passed!"
else
    echo "✗ Some tests failed!"
fi