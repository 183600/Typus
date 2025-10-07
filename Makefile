# Makefile for Typus project

.PHONY: build test clean help install coverage coverage-report

# Default target
help:
	@echo "Typus Project Makefile"
	@echo "======================"
	@echo "Available targets:"
	@echo "  build            - Build the project"
	@echo "  test             - Run all tests"
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
	cabal build
	@echo "Project built successfully"

# Run all tests
test:
	cabal test --test-show-details=always
	@echo "All tests passed!"

# Run quick tests (without performance tests)
test-quick:
	cabal test --test-show-details=always --test-option=--skip=performance
	@echo "Quick tests passed!"

# Run only unit tests
test-unit:
	cabal test --test-show-details=always --test-option=--skip=performance --test-option=--skip=integration
	@echo "Unit tests passed!"

# Run only performance tests
test-performance:
	cabal test --test-show-details=always --test-option=--skip=unit --test-option=--skip=integration
	@echo "Performance tests passed!"

# Run only integration tests
test-integration:
	cabal test --test-show-details=always --test-option=--skip=unit --test-option=--skip=performance
	@echo "Integration tests passed!"

# Generate test coverage report
coverage:
	cabal configure --enable-coverage
	cabal build
	cabal test --test-show-details=always
	@echo "Coverage report generated in dist-newstyle/build/coverage/"

# Open coverage report in browser
coverage-report:
	@echo "Opening coverage report in browser..."
	@python3 -m webbrowser "file://$(PWD)/dist-newstyle/build/coverage/hpc_index.html" 2>/dev/null || echo "Please open dist-newstyle/build/coverage/hpc_index.html manually"

# Clean generated files
clean:
	cabal clean
	rm -rf dist-newstyle
	rm -rf test_temp
	@echo "Cleaned generated files"

# Install dependencies
install:
	cabal configure
	cabal build
	cabal install
	@echo "Dependencies installed"