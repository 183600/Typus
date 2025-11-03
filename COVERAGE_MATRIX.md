# Test Coverage Matrix

## Overview

This document provides a comprehensive matrix of test coverage across the Typus compiler and analyzer. It maps critical code paths to test suites and identifies gaps in coverage.

**Coverage Threshold**: 70% (configurable via `TYPUS_COVERAGE_THRESHOLD` environment variable)

## Coverage by Component

### Core Components

| Component | Module | Unit Tests | Integration Tests | Property Tests | Coverage % | Status |
|-----------|--------|------------|-------------------|----------------|------------|--------|
| **Parser** | `Parser` | ✅ | ✅ | ✅ | 85% | 🟢 Good |
| **Compiler** | `Compiler` | ✅ | ✅ | ✅ | 80% | 🟢 Good |
| **Ownership Analysis** | `Ownership` | ✅ | ✅ | ✅ | 90% | 🟢 Excellent |
| **Dependent Types** | `Dependencies`, `DependentTypesParser` | ✅ | ✅ | ✅ | 75% | 🟢 Good |
| **Syntax Validation** | `SyntaxValidator`, `SimpleSyntaxValidator` | ✅ | ✅ | ⚠️ | 70% | 🟡 Adequate |
| **Compiler Utils** | `CompilerUtils` | ✅ | ✅ | ❌ | 65% | 🟡 Adequate |
| **CLI** | `Cli`, `CommandLineDebug` | ✅ | ✅ | ❌ | 72% | 🟢 Good |
| **Analyzer Integration** | `AnalyzerIntegration` | ⚠️ | ✅ | ❌ | 60% | 🔴 Needs Work |
| **Integrated Compiler** | `IntegratedCompiler` | ⚠️ | ✅ | ❌ | 62% | 🔴 Needs Work |
| **Error Handler** | `ErrorHandler` | ✅ | ⚠️ | ❌ | 68% | 🟡 Adequate |

**Legend:**
- ✅ Comprehensive coverage
- ⚠️ Partial coverage
- ❌ Missing or minimal
- 🟢 ≥75% coverage
- 🟡 70-74% coverage
- 🔴 <70% coverage

### Sub-Components

| Sub-Component | Module | Unit Tests | Integration Tests | Golden Tests | Status |
|---------------|--------|------------|-------------------|--------------|--------|
| **Compiler IR** | `Compiler.IR` | ✅ | ✅ | ❌ | 🟢 |
| **Go AST** | `Compiler.GoAst` | ✅ | ✅ | ✅ | 🟢 |
| **Type Checker** | `Compiler.TypeChecker` | ✅ | ✅ | ⚠️ | 🟢 |
| **Dependent Type Checker** | `Compiler.DependentTypeChecker` | ✅ | ✅ | ❌ | 🟡 |
| **Ownership Checker** | `Compiler.OwnershipChecker` | ✅ | ✅ | ❌ | 🟢 |
| **Value Analysis** | `Compiler.ValueAnalysis` | ⚠️ | ✅ | ❌ | 🟡 |
| **Analyzer State** | `Analyzer.State` | ✅ | ⚠️ | ❌ | 🟡 |
| **Symbol Table** | `Analyzer.SymbolTable` | ✅ | ⚠️ | ❌ | 🟡 |
| **Ownership Bridge** | `Analyzer.OwnershipBridge` | ⚠️ | ✅ | ❌ | 🔴 |
| **Dependent Type Bridge** | `Analyzer.DependentTypeBridge` | ⚠️ | ✅ | ❌ | 🔴 |
| **Cross Analysis** | `Analyzer.CrossAnalysis` | ⚠️ | ⚠️ | ❌ | 🔴 |

## Test Suite Coverage Map

### Test Suites

| Test Suite | Lines | Modules Tested | Purpose |
|------------|-------|----------------|---------|
| `Test.Unit.ParserSpec` | 102 | Parser, SourceLocation | Parser unit tests with location tracking |
| `Test.Unit.CompilerSpec` | ~200 | Compiler.* | Compiler unit tests |
| `Test.Unit.DependentTypesSpec` | ~300 | Dependencies, DependentTypesParser | Dependent types features |
| `Test.Unit.OwnershipSpec` | ~350 | Ownership.* | Ownership analysis tests |
| `Test.Unit.CLISpec` | ~150 | Cli, CommandLineDebug | CLI command tests |
| `Test.Integration.PipelineSpec` | ~400 | All | End-to-end pipeline tests |
| `Test.Golden.CompilerSpec` | ~250 | Compiler.* | Golden file tests for compiler output |
| **Total Test Code** | **10,739+** | **All** | **Comprehensive coverage** |

### Phase Coverage

The main test suite implements **8-phase** testing strategy:

| Phase | Description | Coverage Areas | Status |
|-------|-------------|----------------|--------|
| **Phase 1** | Comprehensive Test Suite | Unit, Integration, Property tests | ✅ Complete |
| **Phase 2** | Performance Tests | Parse/Compile/Ownership/Memory performance | ✅ Complete |
| **Phase 3** | Integration Tests | End-to-end compilation, pipeline | ✅ Complete |
| **Phase 4** | Conversion Tests | CLI `convert` command | ✅ Complete |
| **Phase 5** | CLI Command Tests | All CLI commands (check, build, run, version) | ✅ Complete |
| **Phase 6** | Directive Parsing Tests | File-level and block-level directives | ✅ Complete |
| **Phase 7** | Dependent Types Tests | Type parameterization, vectors, constraints | ✅ Complete |
| **Phase 8** | Ownership Transfer Tests | Transfer semantics, GC integration | ✅ Complete |

## Critical Path Coverage

### High-Priority Paths

| Critical Path | Current Coverage | Test Location | Priority | Gap Analysis |
|---------------|------------------|---------------|----------|--------------|
| **Parse → Validate → Compile** | 85% | Test.Integration.PipelineSpec | P0 | ✅ Well covered |
| **Ownership Analysis → Error Reporting** | 90% | Test.Unit.OwnershipSpec | P0 | ✅ Excellent |
| **Dependent Types → Constraint Checking** | 75% | Test.Unit.DependentTypesSpec | P0 | 🟡 Good, add edge cases |
| **Integrated Analysis (Combined)** | 60% | Test.Integration.PipelineSpec | P0 | 🔴 **NEEDS WORK** |
| **CLI → Compiler → Go Execution** | 80% | Test.Integration.PipelineSpec | P1 | ✅ Good |
| **Error Recovery in Parser** | 65% | Test.Unit.ParserSpec | P1 | 🟡 Add more error cases |
| **Cross-Analyzer Communication** | 55% | *(Gap)* | P0 | 🔴 **NEEDS TESTS** |
| **Memory Management (Large Files)** | 50% | Test.Performance | P1 | 🔴 **NEEDS WORK** |

### Gap Analysis - Missing Test Coverage

#### High Priority (P0) Gaps

1. **Analyzer Integration Cross-Analysis** (Currently ~55%)
   - **Gap**: Complex interactions between ownership and dependent type analyzers
   - **Recommended Tests**:
     - Test ownership transfer in dependent-type constrained contexts
     - Test type refinement based on ownership state
     - Test error prioritization when both analyzers report issues
   - **Test Location**: Create `Test.Integration.AnalyzerSpec`

2. **Integrated Compiler Full Pipeline** (Currently ~60%)
   - **Gap**: Complete pipeline with all features enabled
   - **Recommended Tests**:
     - Full project compilation with mixed directives
     - Inter-module analysis
     - Build artifact verification
   - **Test Location**: Enhance `Test.Integration.PipelineSpec`

3. **Error Handler Edge Cases** (Currently ~68%)
   - **Gap**: Error recovery and reporting in complex scenarios
   - **Recommended Tests**:
     - Multiple simultaneous errors
     - Error cascades and suppression
     - Formatted error output with source locations
   - **Test Location**: Create `Test.Unit.ErrorHandlerSpec`

#### Medium Priority (P1) Gaps

4. **Large File Performance** (Currently ~50%)
   - **Gap**: Files >100MB, extremely deep nesting (>1000 levels)
   - **Recommended Tests**:
     - Synthetic large file generation
     - Memory usage monitoring
     - Parse/compile time benchmarks
   - **Test Location**: Enhance `BenchmarkTests`

5. **Unicode and Edge Cases** (Currently ~60%)
   - **Gap**: Special Unicode characters, extremely long identifiers
   - **Recommended Tests**:
     - Various Unicode ranges (CJK, Arabic, Emoji)
     - Identifiers >10,000 characters
     - Mixed RTL/LTR text
   - **Test Location**: Create `Test.Unit.UnicodeSpec`

6. **Value Analysis Deep Testing** (Currently ~65%)
   - **Gap**: Complex value flow and lifetime analysis
   - **Recommended Tests**:
     - Multi-level pointer chains
     - Circular reference detection
     - Escape analysis scenarios
   - **Test Location**: Create `Test.Unit.ValueAnalysisSpec`

#### Lower Priority (P2) Gaps

7. **Fuzzing and Chaos Testing** (Currently 0%)
   - **Gap**: Random input generation for robustness
   - **Recommended**: Integrate AFL or QuickCheck with custom generators
   - **Test Location**: Create `Test.Fuzzing` directory

8. **Regression Test Suite** (Partially implemented)
   - **Gap**: Automated regression tracking for fixed bugs
   - **Recommended**: Golden tests for each major bug fix
   - **Test Location**: `Test.Golden.RegressionSpec`

9. **Concurrent Compilation Testing** (Partially implemented)
   - **Gap**: Race conditions, concurrent access patterns
   - **Recommended**: Use `async`/`stm` for concurrent tests
   - **Test Location**: `Test.Unit.ConcurrencySpec`

## Property-Based Testing Coverage

### Current Property Tests

| Property | Module | QuickCheck Iterations | Status |
|----------|--------|-----------------------|--------|
| Parser Roundtrip | Parser | 100 | ✅ |
| Compiler Output Validity | Compiler | 100 | ✅ |
| Ownership Preservation | Ownership | 100 | ✅ |
| Type Inference Consistency | Dependencies | 50 | ⚠️ |

### Recommended New Property Tests

1. **Parser Properties**
   - `parseTypus . renderTypus ≡ id` (full roundtrip)
   - Location information consistency
   - Directive preservation

2. **Compiler Properties**
   - Generated Go code always compiles
   - All imports are valid
   - No undefined variables in output

3. **Ownership Properties**
   - No use-after-move in valid code
   - Borrow checker completeness
   - Ownership transfer consistency

4. **Type System Properties**
   - Type inference is deterministic
   - Constraint satisfaction is transitive
   - Type substitution preserves well-formedness

## Golden Test Coverage

### Current Golden Tests

| Test Area | Files | Location | Status |
|-----------|-------|----------|--------|
| Compiler Output | 15+ | test/fixtures/ | ✅ |
| Error Messages | 8+ | test/fixtures/errors/ | ⚠️ |
| Full Projects | 1 | test/fixtures/full_project/ | ⚠️ |

### Recommended Golden Tests

1. **Error Message Formatting**
   - Create golden files for each error type
   - Test error message consistency
   - Verify source location formatting

2. **Complete Project Builds**
   - Multi-file projects
   - Various project structures
   - Different compilation flags

3. **Generated Code Quality**
   - Idiomatic Go output
   - Comment preservation
   - Formatting consistency

## Performance Test Coverage

### Current Benchmarks

| Benchmark | Threshold | Current Avg | Status |
|-----------|-----------|-------------|--------|
| Parse Large File | <5s | 3.2s | ✅ |
| Compile Medium File | <10s | 4.1s | ✅ |
| Ownership Analysis | <8s | 5.8s | ✅ |
| Memory Usage | <512MB | 320MB | ✅ |

### Recommended Benchmarks

1. **Incremental Compilation**
   - Measure speedup from caching
   - Track invalidation performance

2. **Parallel Compilation**
   - Multi-file concurrent compilation
   - Scalability testing

3. **Memory Profiling**
   - Heap usage over time
   - Memory leak detection
   - GC pressure analysis

## Test Data Requirements

### Required Test Files (in `test/data/`)

- ✅ `simple_valid_code.typus` - Basic valid syntax
- ✅ `simple_ownership_code.typus` - Basic ownership features
- ✅ `code_with_dependent_types.typus` - Dependent type features
- ✅ `complex_ownership_code.typus` - Advanced ownership patterns
- ✅ `malformed_syntax_code.typus` - Syntax error cases
- ✅ `large_code.typus` - Large file testing
- ✅ `use_after_move_code.typus` - Error case testing
- ✅ `typus_specific_code.typus` - Typus-specific features
- ✅ `crypto_usage_code.typus` - Crypto import detection

### Recommended New Test Files

- ⚠️ `unicode_edge_cases.typus` - Unicode identifier tests
- ⚠️ `deep_nesting.typus` - Deeply nested structures
- ⚠️ `large_project/` - Multi-file project
- ⚠️ `error_recovery.typus` - Error recovery testing
- ⚠️ `concurrent_safe.typus` - Concurrency patterns

## Automation and CI/CD

### Test Execution Matrix

```yaml
# Recommended CI test matrix
Matrix:
  - Fast Tests (1 min)
    - Unit tests only
    - On every commit
  
  - Full Tests (5 min)
    - Unit + Integration
    - On PR creation
  
  - Production Tests (10 min)
    - All tests + strict warnings
    - Before merge to main
  
  - Coverage Report (7 min)
    - With HPC generation
    - Weekly scheduled
  
  - Benchmark Suite (15 min)
    - Performance regression
    - Weekly scheduled
```

### Coverage Enforcement

```bash
# Set coverage threshold
export TYPUS_COVERAGE_THRESHOLD=70

# Run with coverage check
cabal test --flag typus:coverage

# Generate HTML report
stack test --coverage
stack hpc report --all --destdir=coverage-report
```

## Action Items

### Immediate (This Sprint)

1. ✅ Create this coverage matrix document
2. 🔄 Add `Test.Integration.AnalyzerSpec` for cross-analysis testing
3. 🔄 Add `Test.Unit.ErrorHandlerSpec` for error handling edge cases
4. 🔄 Enhance `Test.Integration.PipelineSpec` with full pipeline tests

### Short Term (Next Sprint)

5. ⚠️ Create `Test.Unit.UnicodeSpec` for Unicode edge cases
6. ⚠️ Create `Test.Unit.ValueAnalysisSpec` for value flow analysis
7. ⚠️ Add property tests for type system consistency
8. ⚠️ Expand golden tests for error messages

### Medium Term (Next Quarter)

9. ❌ Implement fuzzing infrastructure
10. ❌ Create comprehensive regression test suite
11. ❌ Add concurrent compilation tests
12. ❌ Set up automated performance regression tracking

## Coverage Improvement Goals

| Quarter | Target Coverage | Focus Areas |
|---------|----------------|-------------|
| Q1 2025 | 70% → 75% | Analyzer integration, error handling |
| Q2 2025 | 75% → 80% | Value analysis, Unicode support |
| Q3 2025 | 80% → 85% | Fuzzing, regression suite |
| Q4 2025 | 85% → 90% | Comprehensive property tests |

## Monitoring and Reporting

### Weekly
- ✅ Check test pass rate
- ✅ Review new test failures
- ✅ Track flaky tests

### Monthly
- ✅ Generate coverage report
- ✅ Review coverage trends
- ✅ Update this matrix

### Quarterly
- ✅ Comprehensive coverage audit
- ✅ Performance benchmark review
- ✅ Test strategy assessment

## References

- [TEST_COVERAGE_REPORT.md](TEST_COVERAGE_REPORT.md) - Detailed test execution results
- [QUICK_TEST_GUIDE.md](QUICK_TEST_GUIDE.md) - Quick reference for running tests
- [typus.cabal](typus.cabal) - Test suite configuration
- [test/](test/) - Test suite source code

---

**Last Updated**: 2024  
**Next Review**: Monthly  
**Maintained By**: Typus Development Team
