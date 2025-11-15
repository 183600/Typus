# Test Consolidation and Migration Guide

## Summary

As of 2024, the Typus project has consolidated all testing into a single, unified test suite to reduce maintenance costs and improve test infrastructure consistency.

## What Changed

### Removed Test Executables

The following standalone test executables have been removed from the build:

1. **parser-test** → Consolidated into `Test.Unit.ParserSpec`
2. **compiler-test** → Consolidated into `Test.Unit.CompilerSpec`
3. **minimal-compiler-test** → Consolidated into `Test.Unit.CompilerSpec`
4. **typus-compilation-test** → Consolidated into `Test.Integration.PipelineSpec`

### Why This Change Was Made

**Problems with the old approach:**

- **High Maintenance Cost**: Each test executable had its own dependencies, build configuration, and entry point
- **Inconsistent Reporting**: Different executables used different testing frameworks or ad-hoc assertions
- **No Centralized Coverage**: Coverage reports were fragmented across multiple executables
- **Duplicate Logic**: Similar test setup code was repeated in multiple places
- **Poor Discoverability**: New contributors didn't know which executable to run
- **No Test Filtering**: Couldn't selectively run subsets of tests

**Benefits of the new unified approach:**

- ✅ **Single Entry Point**: All tests run via `cabal test`
- ✅ **Consistent Framework**: Everything uses Tasty for structured test organization
- ✅ **Unified Coverage**: Single coverage report across all test types
- ✅ **Better Filtering**: Use Tasty's pattern matching to run specific tests
- ✅ **Shared Infrastructure**: Common test utilities and fixtures
- ✅ **Clear Organization**: Tests organized by type (Unit, Integration, Golden)
- ✅ **Production Readiness**: Built-in support for different test modes (fast/full/production)

## Migration Guide

### For Developers

#### Old Way ❌

```bash
# Old: Running individual test executables
cabal run parser-test
cabal run compiler-test
cabal run minimal-compiler-test
cabal run typus-compilation-test
```

#### New Way ✅

```bash
# Run quick tests (default)
cabal test

# Run specific test modules using patterns
cabal test --test-options="--pattern \"Parser\""
cabal test --test-options="--pattern \"Compiler\""
cabal test --test-options="--pattern \"Integration\""

# Run tests by type
cabal test --test-options="--pattern \"Test.Unit\""
cabal test --test-options="--pattern \"Test.Integration\""
cabal test --test-options="--pattern \"Test.Golden\""

# Run with different modes
cabal test --flags="+fast"             # Quick tests only (explicitly enable fast)
cabal test --flags="-fast full"        # All tests including slow ones
cabal test --flags="-fast production"  # Production-ready tests with strict checks
cabal test --flags="-fast coverage"    # Generate coverage report
```

### For CI/CD Pipelines

#### Old Configuration ❌

```yaml
# Old: Running multiple separate executables
- name: Run parser tests
  run: cabal run parser-test

- name: Run compiler tests
  run: cabal run compiler-test

- name: Run compilation tests
  run: cabal run typus-compilation-test
```

#### New Configuration ✅

```yaml
# New: Single unified test command
- name: Run all tests
  run: cabal test --flags="-fast full" --test-show-details=streaming

# Or use different test modes for different stages
- name: Fast tests on commit
  run: cabal test

- name: Full tests on PR
  run: cabal test --flags="-fast full"

- name: Production tests before merge
  run: cabal test --flags="-fast production coverage"
```

### For Test Writers

#### Adding New Tests

**Old way:** Create a new executable in `app/` and add to `typus.cabal`

**New way:** Add test cases to the appropriate module in `test/`:

```haskell
-- For parser tests: test/Test/Unit/ParserSpec.hs
-- For compiler tests: test/Test/Unit/CompilerSpec.hs
-- For integration tests: test/Test/Integration/PipelineSpec.hs
-- For analyzer integration: test/Test/Integration/AnalyzerSpec.hs
```

Example:

```haskell
-- In test/Test/Unit/ParserSpec.hs
, testCase "my new parser feature" $ do
    let code = "package main\nfunc main() {}"
    case parseTypus code of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right result -> assertBool "Should parse successfully" True
```

## Test Organization

The unified test suite is organized into three main categories:

### 1. Unit Tests (`test/Test/Unit/`)

- `ParserSpec.hs` - Parser functionality
- `CompilerSpec.hs` - Compiler functionality
- `DependentTypesSpec.hs` - Dependent type features
- `OwnershipSpec.hs` - Ownership analysis
- `CLISpec.hs` - Command-line interface

### 2. Integration Tests (`test/Test/Integration/`)

- `PipelineSpec.hs` - End-to-end compilation pipeline
- `AnalyzerSpec.hs` - **NEW**: Integrated analyzer cross-analysis tests

### 3. Golden Tests (`test/Test/Golden/`)

- `CompilerSpec.hs` - Golden file tests for compiler output

## New Features

### Enhanced Coverage Matrix

A new `COVERAGE_MATRIX.md` document has been added to track test coverage across all components:

- Visual coverage matrix showing which components are tested
- Gap analysis identifying missing test coverage
- Prioritized action items for improving coverage
- Performance benchmarks and thresholds

### New Integration Tests for Analyzer

The new `Test.Integration.AnalyzerSpec` module addresses critical gaps in coverage:

- **Cross-Analysis Tests**: Tests for interactions between ownership and dependent type analyzers
- **Error Prioritization**: Tests for proper error ordering and reporting
- **Full Pipeline Integration**: Tests for complete analysis with mixed directives
- **Symbolic State Management**: Tests for symbol table and type environment consistency

These tests specifically target the previously under-tested areas identified in the coverage report:

- AnalyzerIntegration (was ~60%, targeted for improvement)
- IntegratedCompiler (was ~62%, targeted for improvement)
- Cross-analyzer communication (was ~55%, now has dedicated tests)

## Test Flags and Modes

The unified test suite supports multiple testing modes:

| Command | Description | Use Case |
|---------|-------------|----------|
| `cabal test` | Run only fast unit tests (default) | During development |
| `cabal test --flags="-fast full"` | Run all tests including slow ones | Before committing |
| `cabal test --flags="-fast production"` | Enable strict production checks | Before releasing |
| `cabal test --flags="-fast coverage"` | Generate coverage reports | Regular coverage tracking |

## Coverage Goals

| Quarter | Target Coverage | Focus Areas |
|---------|----------------|-------------|
| Q1 2025 | 70% → 75% | Analyzer integration, error handling |
| Q2 2025 | 75% → 80% | Value analysis, Unicode support |
| Q3 2025 | 80% → 85% | Fuzzing, regression suite |
| Q4 2025 | 85% → 90% | Comprehensive property tests |

## Historical Context

The test executables remain in the `app/` directory for historical reference but are no longer built by default. They can still be examined to understand the evolution of the testing approach:

- `app/ParserTest.hs` - Original simple parser test
- `app/CompilerTest.hs` - Original simple compiler test
- `app/MinimalCompilerTest.hs` - Minimal compilation test
- `app/TypusCompilationTest.hs` - Full compilation test

## Frequently Asked Questions

### Q: Can I still run individual test files?

A: Yes, use Tasty's pattern matching:

```bash
cabal test --test-options="--pattern \"Parser\""
```

### Q: How do I debug a specific test?

A: Use pattern matching and streaming output:

```bash
cabal test --test-show-details=streaming --test-options="--pattern \"my test name\""
```

### Q: Where should I add new tests?

A: Add them to the appropriate module in `test/Test/Unit/`, `test/Test/Integration/`, or `test/Test/Golden/` depending on the test type.

### Q: How do I generate coverage reports?

A: Run tests with coverage flag:

```bash
cabal test --flag typus:coverage
# or with stack
stack test --coverage
stack hpc report --all --destdir=coverage-report
```

### Q: Will old CI pipelines break?

A: If your CI was using `cabal run parser-test` etc., yes. Update to use `cabal test` instead. See the migration guide above.

## References

- [COVERAGE_MATRIX.md](COVERAGE_MATRIX.md) - Detailed coverage analysis and gaps
- [TEST_COVERAGE_REPORT.md](TEST_COVERAGE_REPORT.md) - Test execution results
- [QUICK_TEST_GUIDE.md](QUICK_TEST_GUIDE.md) - Quick reference for running tests
- [typus.cabal](typus.cabal) - Test suite configuration

## Feedback

If you have questions or concerns about the test consolidation, please:

1. Check this document first
2. Review the [QUICK_TEST_GUIDE.md](QUICK_TEST_GUIDE.md)
3. Check the [COVERAGE_MATRIX.md](COVERAGE_MATRIX.md) for specific test coverage
4. Open an issue with the `testing` label

---

**Date**: 2024  
**Status**: Complete  
**Impact**: Breaking change for CI/CD pipelines using old test executables
