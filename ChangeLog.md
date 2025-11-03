# Changelog

All notable changes to Typus will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Ownership mechanism with file-level and block-level directives
- Dependent types support with runtime constraint validation
- Refinement types for precise value constraints
- Block-level directives (`//! ownership: on`, `//! constraints: on`)
- File-level directives for enabling features per-file
- `TYPUS_SKIP_GO_BUILD` environment variable for pure transpilation mode
- Comprehensive test suite with coverage reporting
- Support for `//go:embed` directive mirroring

### Compiler Features
- Convert single files or entire directories to Go
- Syntax checking and validation
- Build and run commands with Go toolchain integration
- Import discovery and automatic import generation
- Type promotion and structural code transformations

### Documentation
- Quick test guide (`QUICK_TEST_GUIDE.md`)
- Debug guide (`DEBUG_GUIDE.md`)
- Test coverage matrix (`COVERAGE_MATRIX.md`)
- Production readiness checklist (`PRODUCTION_READINESS.md`)

### Testing
- Unit tests for parser, compiler, and type system
- Integration tests for ownership analysis
- End-to-end compilation tests
- Coverage reporting support

## [0.1.0] - Initial Release

### Added
- Initial Typus compiler implementation
- Go-like syntax with ownership extensions
- Basic type system
- Command-line interface with `convert`, `check`, `build`, and `run` commands
