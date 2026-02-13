#!/usr/bin/env python3

# Create a simple debug script to understand what's happening
import subprocess
import os

# Create a temporary debug version of Compiler.hs
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'r') as f:
    content = f.read()

# Add debug trace to generateImports function
old_line = '    hasRuntime = "runtime." `isInfixOf` content || "\\\"runtime\\\"" `isInfixOf` content || "runtime" `isPrefixOf` content || " runtime" `isInfixOf` content || "\\truntime" `isInfixOf` content'
new_line = '    debug_content = "DEBUG CONTENT: " ++ (take 300 content)\n    _ = trace debug_content ()\n    hasRuntime = "runtime." `isInfixOf` content || "\\\"runtime\\\"" `isInfixOf` content || "runtime" `isPrefixOf` content || " runtime" `isInfixOf` content || "\\truntime" `isInfixOf` content\n    debug_runtime = "DEBUG hasRuntime: " ++ show hasRuntime\n    _ = trace debug_runtime ()'

content = content.replace(old_line, new_line)

# Write the modified version
with open('/home/qwe12345678/typus2/src/Compiler.hs', 'w') as f:
    f.write(content)

print("Added debug traces to Compiler.hs")

# Build and test
os.chdir('/home/qwe12345678/typus2')
subprocess.run(['cabal', 'build'], check=True)
result = subprocess.run(['typus', 'convert', 'examples/test_no_import.typus', '-o', 'examples/debug_test.go'], 
                       capture_output=True, text=True)

print("\n=== TYPUS OUTPUT ===")
print("STDOUT:")
print(result.stdout)
print("STDERR:")
print(result.stderr)