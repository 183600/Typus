# Memory Optimization for Typus Tests

This document describes the memory optimization strategies implemented for the Typus test suite to reduce memory consumption while preserving test coverage.

## Overview

The Typus test suite includes numerous QuickCheck property tests that can consume significant memory. To address this, we've implemented multiple levels of memory optimization.

## Memory Optimization Levels

### 1. Standard Memory Limits
- **Script**: `./scripts/run_tests.sh`
- **Memory Limit**: Default system limits
- **QuickCheck Tests**: 100
- **QuickCheck Max Size**: 20

### 2. Memory-Optimized (Recommended)
- **Script**: `./scripts/run_memory_optimized_tests.sh`
- **Memory Limit**: 256MB
- **Allocation Area**: 16MB
- **Nursery Size**: 2MB
- **QuickCheck Tests**: 25
- **QuickCheck Max Size**: 10

### 3. Ultra Memory-Optimized (Severely Constrained)
- **Script**: `./scripts/run_ultra_memory_optimized_tests.sh`
- **Memory Limit**: 128MB
- **Allocation Area**: 8MB
- **Nursery Size**: 1MB
- **QuickCheck Tests**: 10
- **QuickCheck Max Size**: 5

## Implementation Details

### Memory Limits Module (`test/TestSupport/MemoryLimits.hs`)

The module provides several functions for applying memory limits:

- `withMemoryLimits`: Moderate limits (100 tests, max size 20)
- `withAggressiveMemoryLimits`: Aggressive limits (50 tests, max size 10)
- `memoryLimitedTestGroup`: Apply moderate limits to test groups
- `aggressiveMemoryLimitedTestGroup`: Apply aggressive limits to test groups

### QuickCheck Module (`test/TestSupport/QuickCheck.hs`)

Enhanced with memory-efficient property wrappers:

- `memoryEfficientProperty`: Standard memory optimization
- `ultraMemoryEfficientProperty`: Ultra memory optimization
- `stringProcessingProperty`: Specialized for string processing tests

## Usage

### Running Memory-Optimized Tests

```bash
# Standard memory-optimized tests
./scripts/run_memory_optimized_tests.sh

# Ultra memory-optimized tests (for severely constrained environments)
./scripts/run_ultra_memory_optimized_tests.sh

# Standard tests (no memory limits)
./scripts/run_tests.sh
```

### Using Memory Limits in Test Code

```haskell
import TestSupport.MemoryLimits (withAggressiveMemoryLimits, aggressiveMemoryLimitedTestGroup)

-- Apply to individual tests
myTest = withAggressiveMemoryLimits $ testCase "my test" myAssertion

-- Apply to test groups
myTestGroup = aggressiveMemoryLimitedTestGroup "My Tests" [test1, test2, test3]
```

## Environment Variables

The following environment variables can be set to further optimize memory usage:

- `GHCRTS`: GHC runtime options for memory management
- `TYPUS_SKIP_GO_BUILD`: Skip Go toolchain initialization (saves memory)
- `GHC_HEAP_ALLOCATION`: Heap allocation ratio
- `GHC_GC_YIELD_LIMIT`: GC yield limit

## Recommendations

1. **For CI/CD pipelines**: Use `run_memory_optimized_tests.sh`
2. **For development**: Use standard tests unless memory is constrained
3. **For resource-limited environments**: Use `run_ultra_memory_optimized_tests.sh`
4. **For debugging**: Use standard tests for full coverage

## Trade-offs

Memory optimization involves trade-offs:

- **Reduced test count**: Fewer QuickCheck iterations
- **Smaller test data**: Limited maximum size for generated data
- **Potential reduced coverage**: Edge cases with large inputs may not be tested

However, the core functionality and most common use cases remain tested.

## Future Improvements

Potential areas for further memory optimization:

1. Parallel test execution with memory isolation
2. Test-specific memory profiles
3. Dynamic memory adjustment based on available resources
4. Selective test execution based on changes