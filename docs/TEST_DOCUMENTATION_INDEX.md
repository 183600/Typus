# Typus Test Documentation Index

The Typus repository ships a collection of test-related documents. This index
summarises their intent so that contributors can quickly locate the right level
of detail for their workflow.

## Quick entry points

- [`QUICK_TEST_GUIDE.md`](../QUICK_TEST_GUIDE.md) – minimal commands for running
  the fast unit suite during development.
- [`TESTING_GUIDE.md`](../TESTING_GUIDE.md) – an in-depth overview of the test
  strategy, environment variables, and troubleshooting tips.

## Coverage and consolidation

- [`COVERAGE_MATRIX.md`](../COVERAGE_MATRIX.md) – **NEW**: comprehensive test
  coverage matrix showing which components are tested, gaps, and action items.
- [`TEST_CONSOLIDATION.md`](../TEST_CONSOLIDATION.md) – **NEW**: migration guide
  explaining the 2024 test consolidation and how to adapt existing workflows.
- [`TEST_COVERAGE_REPORT.md`](../TEST_COVERAGE_REPORT.md) – the latest captured
  Haskell coverage snapshot together with interpretation guidelines.
- [`TEST_ENHANCEMENT_SUMMARY.md`](../TEST_ENHANCEMENT_SUMMARY.md) – changelog of
  major test harness improvements and rationale behind larger refactors.

## Supplementary material

- [`DEBUG_GUIDE.md`](../DEBUG_GUIDE.md) and [`debug-example.md`](../debug-example.md)
  – worked examples of tracing failing conversions and common debugging
  techniques.
- [`README_COMPREHENSIVE_TEST.md`](../README_COMPREHENSIVE_TEST.md) – documentation
  for the historical "comprehensive" suite that is still useful as reference
  when porting legacy scenarios into the new structure.

## Generated artefacts

Large binaries are no longer stored in the repository. Use the helper script to
rebuild them on demand:

```bash
./scripts/regenerate-example-artifacts.sh
```

The script emits Go sources into `examples/generated/`, keeping the Git history
compact while allowing developers to inspect the generated output when needed.
