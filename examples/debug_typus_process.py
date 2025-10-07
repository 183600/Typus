#!/usr/bin/env python3

# Debug what's happening in the typus compilation process
import subprocess

# Run typus with debug output
result = subprocess.run([
    'cd', '/home/qwe12345678/typus2', '&&', 
    'typus', 'convert', 'examples/simple_test.typus', '-o', 'examples/debug_output.go'
], shell=True, capture_output=True, text=True)

print("STDOUT:")
print(result.stdout)
print("\nSTDERR:")
print(result.stderr)
print(f"\nReturn code: {result.returncode}")

# Read the original file and the generated file
print("\n=== ORIGINAL FILE ===")
with open('/home/qwe12345678/typus2/examples/simple_test.typus', 'r') as f:
    original = f.read()
print(original)

print("\n=== GENERATED FILE ===")
with open('/home/qwe12345678/typus2/examples/debug_output.go', 'r') as f:
    generated = f.read()
print(generated)

# Analyze what's happening
print("\n=== ANALYSIS ===")
print("Original contains runtime import:", '"runtime"' in original)
print("Original contains runtime.:", 'runtime.' in original)
print("Generated contains runtime in import block:", '    "runtime"' in generated)
print("Generated contains stray runtime import:", '\t"runtime"' in generated or '"runtime"' in generated.split('import (')[1].split(')')[0] if 'import (' in generated else False)