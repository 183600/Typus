#!/bin/bash

# Comprehensive Typus Compiler Test Script
# This script will:
# 1. Find all Typus files in the project
# 2. Compile each Typus file to Go code
# 3. Check if compilation succeeds (if not, test fails)
# 4. Run each generated Go file with 'go run'
# 5. Check if execution succeeds (if not, test fails)

set -e  # Exit on any error

echo "=== Comprehensive Typus Compiler Test ==="
echo ""

# Build the typus compiler first
echo "Step 0: Building the typus compiler..."
stack build --flag typus:werror
echo "✅ Typus compiler built successfully!"
echo ""

# Find all .typus files
echo "Step 1: Finding all .typus files..."
TYPUS_FILES=$(find .. -name "*.typus" -type f | grep -v ".stack-work" | grep -v "dist-newstyle" | grep -v ".git" | grep -v "tmp" | grep -v "temp" || true)

# Count files
FILE_COUNT=$(echo "$TYPUS_FILES" | wc -l)
echo "Found $FILE_COUNT .typus files to test"

if [ "$FILE_COUNT" -eq 0 ]; then
    echo "No .typus files found!"
    exit 1
fi

# Track test results
PASSED=0
FAILED=0
FAILED_FILES=""

# Test each file
echo ""
echo "Step 2: Testing each Typus file..."
echo ""

for typus_file in $TYPUS_FILES; do
    echo "=== Testing: $typus_file ==="

    # Skip if file doesn't exist
    if [ ! -f "$typus_file" ]; then
        echo "❌ File does not exist: $typus_file"
        FAILED=$((FAILED + 1))
        FAILED_FILES="$FAILED_FILES $typus_file"
        continue
    fi

    # Create temporary directory for this test
    TEMP_DIR=$(mktemp -d typus_test_XXXXXX)

    # Generate safe filename
    BASE_NAME=$(basename "$typus_file" .typus)
    SAFE_NAME=$(echo "$BASE_NAME" | sed 's/test/exec/g')
    GO_FILE="$TEMP_DIR/$SAFE_NAME.go"

    # Step 2.1: Compile Typus to Go
    echo "  Compiling to Go..."
    if ! typus convert "$typus_file" "$GO_FILE" 2>/dev/null; then
        echo "  ❌ COMPILATION FAILED"
        rm -rf "$TEMP_DIR"
        FAILED=$((FAILED + 1))
        FAILED_FILES="$FAILED_FILES $typus_file"
        continue
    fi

    # Check if Go file was created and is not empty
    if [ ! -s "$GO_FILE" ]; then
        echo "  ❌ COMPILATION FAILED: Empty Go file generated"
        rm -rf "$TEMP_DIR"
        FAILED=$((FAILED + 1))
        FAILED_FILES="$FAILED_FILES $typus_file"
        continue
    fi

    echo "  ✅ Compilation successful!"

    # Step 2.2: Create go.mod file
    echo "  Creating go.mod..."
    cat > "$TEMP_DIR/go.mod" << EOF
module temp

go 1.21
EOF

    # Step 2.3: Run the Go code
    echo "  Running Go code..."
    if timeout 30s go run "$GO_FILE" >/dev/null 2>&1; then
        echo "  ✅ Execution successful!"
        PASSED=$((PASSED + 1))
    else
        echo "  ❌ EXECUTION FAILED"
        FAILED=$((FAILED + 1))
        FAILED_FILES="$FAILED_FILES $typus_file"
    fi

    # Clean up
    rm -rf "$TEMP_DIR"
    echo ""
done

# Report final results
echo "=== Test Results ==="
echo "Total files tested: $FILE_COUNT"
echo "Passed: $PASSED"
echo "Failed: $FAILED"

if [ "$FAILED" -gt 0 ]; then
    echo ""
    echo "❌ Some tests failed!"
    echo "Failed files:"
    for file in $FAILED_FILES; do
        echo "  - $file"
    done
    exit 1
else
    echo ""
    echo "✅ All tests passed!"
    exit 0
fi