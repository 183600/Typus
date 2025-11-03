# Typus Fixtures and Test Resources

This directory contains test files, debug scripts, and reference outputs for the Typus compiler project.

## Directory Structure

### `test-cases/`
Contains `.typus` test programs and supporting `.txt` inputs used for unit testing, integration testing, and manual verification of the compiler.

**Categories:**

- **Date-stamped Tests** (`250919.typus`, `250921.typus`, etc.): Historical test cases capturing specific bug scenarios or features from development
- **Comprehensive Tests** (`all_syntax.typus`, `comprehensive_go_*.typus`): Large test files exercising a wide range of language features
- **Debug Tests** (`debug*.typus`): Minimal test cases for debugging specific compiler phases
- **Feature Tests**:
  - `ownership`: `test_ownership*.typus`, `test_borrow.typus` - Ownership mechanism tests
  - `dependent_types`: Tests for dependent type and refinement type features
  - `control_flow`: `test_control_flow*.typus` - Control flow analysis tests
  - `closures`: `closure_test.typus`, `test_closure*.typus` - Closure handling
- **Edge Cases** (`edge_cases.typus`, `complex_features.typus`): Corner cases and complex scenarios
- **Minimal Tests** (`minimal*.typus`, `simple*.typus`, `temp_test*.typus`): Small focused tests for quick verification

### `debug-scripts/`
Contains Haskell debug scripts and utilities for manual testing and debugging.

**Categories:**

- **CLI Debug Tools** (`debug-cli.hs`, `debug-cmd.hs`): Interactive debugging command-line tools
- **Ownership Analysis** (`debug_ownership*.hs`, `test_ownership*.hs`): Scripts testing ownership tracking
- **Parser Tests** (`debug_parser.hs`, `test_parse*.hs`): Parser debugging utilities
- **Control Flow** (`test_control_flow*.hs`): Control flow analysis testing
- **Misc Debug** (`debug_*.hs`, `tmp_*.hs`): Various debugging and exploration scripts

### `reference-output/`
Contains reference text files showing expected behavior and comparisons.

**Files:**
- `comparison.txt`: Feature comparisons
- `*_original.txt`, `*_typus.txt`: Side-by-side comparisons of original vs. Typus output
- `go_syntax_diff.txt`: Go syntax differences
- `ghci_commands.txt`, `ghci_commands2.txt`: GHCI REPL commands for testing

### `logs/`
Contains test output logs and historical test results.

**Files:**
- `original_output.txt`, `output.txt`, `typus_output.txt`: Compiler output logs
- `stack_test*.txt`: Stack test run outputs
- `debug_output.txt`: Debug session logs

## Using Test Cases

### Running Individual Tests

```bash
# Convert a test file
typus convert fixtures/test-cases/simple.typus -o test_output.go

# Check syntax
typus check fixtures/test-cases/test_ownership.typus

# Run comprehensive test
typus run fixtures/test-cases/comprehensive_go_test.typus
```

### Running Debug Scripts

```bash
# Run ownership analysis test
runhaskell fixtures/debug-scripts/test_ownership_analysis.hs

# Use the debug CLI
runhaskell fixtures/debug-scripts/debug-cli.hs

# Test parser with specific input
runhaskell fixtures/debug-scripts/debug_parser.hs
```

### Adding New Test Cases

When adding new test files:

1. **Naming conventions:**
   - `test_<feature>_<variant>.typus` for feature tests
   - `debug_<issue>.typus` for debugging specific issues
   - `<date>.typus` for historical bug reproductions

2. **Documentation:**
   - Add a comment at the top of the file explaining its purpose
   - Reference any related issues or PRs

3. **Organization:**
   - Place in `test-cases/` if it's a .typus test file
   - Place in `debug-scripts/` if it's a Haskell debug utility

## Test Case Index

### Key Test Files

| File | Purpose |
|------|---------|
| `all_syntax.typus` | Comprehensive syntax coverage |
| `test_ownership.typus` | Core ownership feature tests |
| `test_control_flow_comprehensive.typus` | Control flow analysis |
| `comprehensive_go_syntax_complete.typus` | Full Go syntax compatibility |
| `edge_cases.typus` | Edge case handling |
| `simple_example.typus` | Basic "hello world" example |

### Debug Script Index

| Script | Purpose |
|--------|---------|
| `debug-cli.hs` | Interactive debugging CLI |
| `test_ownership_analysis.hs` | Ownership tracking verification |
| `test_control_flow_final.hs` | Control flow test suite |
| `debug_parser.hs` | Parser debugging tool |

## Integration with Test Suite

The Haskell test suite (under `test/`) references many of these fixtures. When modifying test files:

1. Run the full test suite: `cabal test`
2. Check for references: `grep -r "fixtures/test-cases/filename.typus" test/`
3. Update any affected tests

## See Also

- [QUICK_TEST_GUIDE.md](../QUICK_TEST_GUIDE.md) - Quick testing workflow
- [TESTING_GUIDE.md](../TESTING_GUIDE.md) - Comprehensive testing documentation
- [DEBUG_GUIDE.md](../DEBUG_GUIDE.md) - Debugging guide
- [docs/TEST_DOCUMENTATION_INDEX.md](../docs/TEST_DOCUMENTATION_INDEX.md) - Test documentation index
