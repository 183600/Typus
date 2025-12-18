#!/usr/bin/env python3

# Test the complete flow to understand where the issue is
import subprocess
import os

# Change to typus2 directory
os.chdir('/home/qwe12345678/typus2')

# Read the simple_test.typus file
with open('examples/simple_test.typus', 'r') as f:
    original_content = f.read()

print("=== ORIGINAL CONTENT ===")
print(original_content)
print("=" * 50)

# Simulate what generateImports should see
content_for_analysis = original_content  # This should be what generateImports receives

# Test generateImports logic
hasRuntime = "runtime." in content_for_analysis or '"runtime"' in content_for_analysis
print(f"hasRuntime: {hasRuntime}")

if hasRuntime:
    runtime_import = '    "runtime"'
    print(f"Should include runtime import: {runtime_import}")
else:
    print("Should NOT include runtime import")

print("=" * 50)

# Now run typus and see what it actually generates
result = subprocess.run(['typus', 'convert', 'examples/simple_test.typus', '-o', 'examples/debug_flow.go'], 
                       capture_output=True, text=True)

print("=== TYPUS OUTPUT ===")
print("STDOUT:", result.stdout)
print("STDERR:", result.stderr)
print("=" * 50)

# Read the generated file
with open('examples/debug_flow.go', 'r') as f:
    generated_content = f.read()

print("=== GENERATED CONTENT ===")
print(generated_content)
print("=" * 50)

# Analyze the generated content
lines = generated_content.split('\n')
import_section_started = False
import_section_ended = False
runtime_in_import_block = False
stray_runtime = None

for i, line in enumerate(lines):
    if line.strip() == 'import (':
        import_section_started = True
        print(f"Line {i}: Import block started")
    elif import_section_started and not import_section_ended and line.strip() == ')':
        import_section_ended = True
        print(f"Line {i}: Import block ended")
    elif import_section_started and not import_section_ended:
        if 'runtime' in line:
            runtime_in_import_block = True
            print(f"Line {i}: Found runtime in import block: {repr(line)}")
    elif not import_section_started or import_section_ended:
        if 'runtime' in line and line.strip() != '':
            stray_runtime = (i, line)
            print(f"Line {i}: Found stray runtime: {repr(line)}")

print("\n=== SUMMARY ===")
print(f"Runtime in import block: {runtime_in_import_block}")
print(f"Stray runtime found: {stray_runtime}")

if stray_runtime:
    print(f"ISSUE: Runtime import found outside import block at line {stray_runtime[0]}: {repr(stray_runtime[1])}")