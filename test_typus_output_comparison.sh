#!/bin/bash

echo "=== Typus Conversion and Go Output Comparison Test ==="

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

# Step 4: Test running converted Go files and compare outputs
echo "Step 4: Testing converted Go files and comparing outputs..."
success_count=0
total_count=0
mismatch_count=0
failed_count=0

for typus_file in "${typus_files[@]}"; do
    base_name=$(basename "$typus_file" .typus)
    go_file="$output_dir/${base_name}.go"

    # Create temporary files for output comparison
    original_output="/tmp/original_${base_name}.out"
    converted_output="/tmp/converted_${base_name}.out"

    if [ -f "$go_file" ]; then
        echo "Testing Go file: $(basename "$go_file")"
        total_count=$((total_count + 1))

        # Find corresponding Go file in input directory for comparison
        original_go_file="$input_dir/${base_name}.go"

        if [ -f "$original_go_file" ]; then
            # Run original Go file and capture output
            if timeout 10s go run "$original_go_file" > "$original_output" 2>&1; then
                echo "  ✓ Original Go file executed successfully"

                # Run converted Go file and capture output
                if timeout 10s go run "$go_file" > "$converted_output" 2>&1; then
                    echo "  ✓ Converted Go file executed successfully"
                    success_count=$((success_count + 1))

                    # Compare outputs
                    if diff -q "$original_output" "$converted_output" >/dev/null 2>&1; then
                        echo "  ✓ Outputs are identical"
                    else
                        echo "  ✗ Outputs differ"
                        mismatch_count=$((mismatch_count + 1))
                        echo "    Original output:"
                        cat "$original_output" | sed 's/^/      /'
                        echo "    Converted output:"
                        cat "$converted_output" | sed 's/^/      /'
                    fi
                else
                    echo "  ✗ Converted Go file execution failed"
                    failed_count=$((failed_count + 1))
                    echo "    Error output:"
                    cat "$converted_output" | sed 's/^/      /'
                fi
            else
                echo "  ✗ Original Go file execution failed"
                failed_count=$((failed_count + 1))
                echo "    Error output:"
                cat "$original_output" | sed 's/^/      /'
            fi
        else
            echo "  WARNING: No corresponding Go file found in input directory: $original_go_file"
            echo "  Testing converted file standalone..."

            # Test standalone converted file
            if timeout 10s go run "$go_file" > "$converted_output" 2>&1; then
                echo "  ✓ Converted Go file executed successfully (standalone)"
                success_count=$((success_count + 1))
            else
                echo "  ✗ Converted Go file execution failed (standalone)"
                failed_count=$((failed_count + 1))
                echo "    Error output:"
                cat "$converted_output" | sed 's/^/      /'
            fi
        fi

        # Clean up temporary files
        rm -f "$original_output" "$converted_output"
    else
        echo "ERROR: Go file not found: $go_file"
        failed_count=$((failed_count + 1))
    fi

    echo ""
done

# Step 5: Summary
echo "=== Test Summary ==="
echo "Total typus files: ${#typus_files[@]}"
echo "Go files tested: $total_count"
echo "Go files that run without error: $success_count"
echo "Go files that failed: $failed_count"
echo "Files with output mismatch: $mismatch_count"

if [ $failed_count -eq 0 ] && [ $mismatch_count -eq 0 ]; then
    echo "✓ All tests passed with identical outputs!"
    exit 0
elif [ $failed_count -eq 0 ]; then
    echo "⚠ All files ran successfully but some outputs differ"
    exit 1
else
    echo "✗ Some tests failed!"
    exit 1
fi