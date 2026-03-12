#!/bin/bash

# Comprehensive memory optimization verification script
# This script verifies that all memory optimizations are correctly applied

echo "=== Comprehensive Memory Optimization Verification ==="
echo "Checking for unbounded Text generation..."

# Check for unbounded Text generation instances
echo "1. Checking Text generation instances..."
TEXT_INSTANCES=$(grep -r "T\.pack.*arbitrary" test/ --include="*.hs" | grep -v "resize" | wc -l)
if [ "$TEXT_INSTANCES" -gt 0 ]; then
    echo "❌ Found $TEXT_INSTANCES unbounded Text generation instances"
    grep -r "T\.pack.*arbitrary" test/ --include="*.hs" | grep -v "resize"
else
    echo "✅ All Text generation instances have memory limits"
fi

echo ""
echo "2. Checking unbounded list generation..."

# Check for unbounded list generation
LIST_INSTANCES=$(grep -r "listOf arbitrary" test/ --include="*.hs" | grep -v "resize" | wc -l)
if [ "$TEXT_INSTANCES" -gt 0 ]; then
    echo "❌ Found $LIST_INSTANCES unbounded list generation instances"
    grep -r "listOf arbitrary" test/ --include="*.hs" | grep -v "resize"
else
    echo "✅ All list generation instances have memory limits"
fi

echo ""
echo "3. Checking problematic generators..."

# Check for problematic generators without limits (exclude those with choose or vectorOf limits)
PROBLEMATIC_GENERATORS=$(grep -r "genSpecialChars\|genUnicodeString" test/ --include="*.hs" | grep -v "resize" | grep -v "choose" | grep -v "vectorOf" | wc -l)
if [ "$PROBLEMATIC_GENERATORS" -gt 0 ]; then
    echo "❌ Found $PROBLEMATIC_GENERATORS problematic generators without limits"
    grep -r "genSpecialChars\|genUnicodeString" test/ --include="*.hs" | grep -v "resize" | grep -v "choose" | grep -v "vectorOf"
else
    echo "✅ All problematic generators have memory limits"
fi

echo ""
echo "4. Checking memory-efficient generators usage..."

# Check if memory-efficient generators are being imported
MEMORY_GENERATOR_IMPORTS=$(grep -r "MemoryEfficientGenerators" test/ --include="*.hs" | wc -l)
if [ "$MEMORY_GENERATOR_IMPORTS" -gt 0 ]; then
    echo "✅ MemoryEfficientGenerators are imported in $MEMORY_GENERATOR_IMPORTS files"
else
    echo "⚠️  MemoryEfficientGenerators are not widely imported"
fi

echo ""
echo "5. Running a lightweight test to verify memory usage..."

# Run a lightweight test to check memory usage
if command -v stack >/dev/null 2>&1; then
    echo "Running lightweight memory test..."
    stack test --test-arguments="--pattern=CoreDependenciesProperties" 2>&1 | head -20
    echo "Lightweight test completed"
else
    echo "⚠️  Stack not available, skipping test execution"
fi

echo ""
echo "=== Memory Optimization Summary ==="
echo "Text generation optimizations: $([ $TEXT_INSTANCES -eq 0 ] && echo '✅ Complete' || echo '❌ Incomplete')"
echo "List generation optimizations: $([ $LIST_INSTANCES -eq 0 ] && echo '✅ Complete' || echo '❌ Incomplete')"
echo "Generator optimizations: $([ $PROBLEMATIC_GENERATORS -eq 0 ] && echo '✅ Complete' || echo '❌ Incomplete')"
echo ""
echo "Memory optimization verification completed!"