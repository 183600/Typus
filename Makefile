# Makefile for Typus project

.PHONY: build test clean help install coverage coverage-report stack-test

# Default target
help:
	@echo "Typus Project Makefile"
	@echo "======================"
	@echo "Available targets:"
	@echo "  build            - Build the project"
	@echo "  test             - Run all tests"
	@echo "  stack-test       - Run all tests via Stack (uses flags in stack.yaml)"
	@echo "  test-quick       - Run quick tests without performance tests"
	@echo "  test-unit        - Run only unit tests"
	@echo "  test-performance - Run only performance tests"
	@echo "  test-integration - Run only integration tests"
	@echo "  coverage         - Generate test coverage report"
	@echo "  coverage-report  - Open coverage report in browser"
	@echo "  clean            - Clean generated files"
	@echo "  install          - Install dependencies"
	@echo "  help             - Show this help message"

# Build the project
build:
	LC_ALL=C cabal build
	@echo "Project built successfully"

# Run all tests
test:
	LC_ALL=C cabal test --test-show-details=always
	@echo "All tests passed!"

# Run all tests via Stack (production-grade flags set in stack.yaml)
stack-test:
	LC_ALL=C stack test --coverage --test-arguments="--hide-successes"
	scripts/coverage-report.sh
	@echo "Stack tests with coverage completed!"

# Run quick tests (without performance tests)
test-quick:
	LC_ALL=C cabal test --test-show-details=always --test-option=--skip=performance
	@echo "Quick tests passed!"

# Run only unit tests
test-unit:
	LC_ALL=C cabal test --test-show-details=always --test-option=--skip=performance --test-option=--skip=integration
	@echo "Unit tests passed!"

# Run only performance tests
test-performance:
	LC_ALL=C cabal test --test-show-details=always --test-option=--skip=unit --test-option=--skip=integration
	@echo "Performance tests passed!"

# Run only integration tests
test-integration:
	LC_ALL=C cabal test --test-show-details=always --test-option=--skip=unit --test-option=--skip=performance
	@echo "Integration tests passed!"

# Generate test coverage report
coverage:
	LC_ALL=C cabal configure --enable-coverage
	LC_ALL=C cabal build
	LC_ALL=C cabal test --test-show-details=always
	@echo "Coverage report generated in dist-newstyle/build/coverage/"

# Open coverage report in browser
coverage-report:
	@echo "Opening coverage report in browser..."
	@python3 -m webbrowser "file://$(PWD)/dist-newstyle/build/coverage/hpc_index.html" 2>/dev/null || echo "Please open dist-newstyle/build/coverage/hpc_index.html manually"

# Clean generated files
clean:
	LC_ALL=C cabal clean
	rm -rf dist-newstyle
	rm -rf test_temp
	@echo "Cleaned generated files"

# Install dependencies
install:
	LC_ALL=C cabal configure
	LC_ALL=C cabal build
	LC_ALL=C cabal install
	@echo "Dependencies installed"

# Run production-grade tests (strict mode with warnings as errors)
test-production:
	LC_ALL=C cabal test --flags="-fast production" --test-show-details=direct
	@echo "Production tests completed!"