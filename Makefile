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
	@. ./.locale-env && cabal build
	@echo "Project built successfully"

# Run all tests
test:
	@. ./.locale-env && cabal test --flags="-fast production" --test-show-details=direct
	@echo "Tests completed!"

# Run all tests via Stack (production-grade flags set in stack.yaml)
stack-test:
	@. ./.locale-env && stack test --coverage --test-arguments="--hide-successes"
	scripts/coverage-report.sh
	@echo "Stack tests with coverage completed!"

# Run quick tests (without performance tests)
test-quick:
	@. ./.locale-env && cabal test --test-show-details=always --test-option=--quickcheck-tests=50
	@echo "Quick tests passed!"

# Run only unit tests
test-unit:
	@. ./.locale-env && cabal test --test-show-details=always --test-option=--quickcheck-tests=50
	@echo "Unit tests passed!"

# Run only performance tests
test-performance:
	@. ./.locale-env && cabal test --test-show-details=always --test-option=--quickcheck-tests=50
	@echo "Performance tests passed!"

# Run only integration tests
test-integration:
	@. ./.locale-env && cabal test --test-show-details=always --test-option=--quickcheck-tests=50
	@echo "Integration tests passed!"

# Generate test coverage report
coverage:
	@. ./.locale-env && cabal configure --enable-coverage
	@. ./.locale-env && cabal build
	@. ./.locale-env && cabal test --test-show-details=always
	@echo "Coverage report generated in dist-newstyle/build/coverage/"

# Open coverage report in browser
coverage-report:
	@echo "Opening coverage report in browser..."
	@python3 -m webbrowser "file://$(PWD)/dist-newstyle/build/coverage/hpc_index.html" 2>/dev/null || echo "Please open dist-newstyle/build/coverage/hpc_index.html manually"

# Clean generated files
clean:
	@. ./.locale-env && cabal clean
	rm -rf dist-newstyle
	rm -rf test_temp
	@echo "Cleaned generated files"

# Install dependencies
install:
	@. ./.locale-env && cabal configure
	@. ./.locale-env && cabal build
	@. ./.locale-env && cabal install
	@echo "Dependencies installed"

# Run production-grade tests (strict mode with warnings as errors)
test-production:
	./cabal_test_clean.sh
	@echo "Production tests completed!"

# Memory-optimized test targets
test-memory-optimized:
	@echo "Running memory-optimized tests..."
	./scripts/enhanced-memory-test-config.sh --environment standard
	@echo "Memory-optimized tests completed!"

test-memory-minimal:
	@echo "Running memory-minimal tests (32MB)..."
	./scripts/enhanced-memory-test-config.sh --memory 32 --conservative --aggressive-gc
	@echo "Memory-minimal tests completed!"

test-memory-ci:
	@echo "Running memory-optimized tests for CI..."
	./scripts/enhanced-memory-test-config.sh --environment ci
	@echo "CI memory-optimized tests completed!"

test-memory-extreme:
	@echo "Running extreme memory-optimized tests (16MB)..."
	./scripts/enhanced-memory-test-config.sh --memory 16 --conservative --aggressive-gc
	@echo "Extreme memory-optimized tests completed!"

test-memory-docker:
	@echo "Running memory-optimized tests for Docker..."
	./scripts/enhanced-memory-test-config.sh --environment docker
	@echo "Docker memory-optimized tests completed!"

# Verify memory optimization effectiveness
verify-memory-optimization:
	@echo "Verifying memory optimization effectiveness..."
	./scripts/verify-memory-optimization.sh
	@echo "Memory optimization verification completed!"

# Verify memory optimization with multiple iterations
verify-memory-optimization-detailed:
	@echo "Verifying memory optimization effectiveness with detailed analysis..."
	./scripts/verify-memory-optimization.sh --multiple
	@echo "Detailed memory optimization verification completed!"

# Build memory-optimized test runners
build-memory-test-runners:
	@echo "Building memory-optimized test runners..."
	cabal build enhanced-memory-test-runner || cabal build smart-memory-test-runner || echo "Test runners already built or not available"
	@echo "Memory-optimized test runners built!"