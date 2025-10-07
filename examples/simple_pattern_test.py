#!/usr/bin/env python3

# Simple test to check the pattern matching logic
content = '''package main

import (
	"fmt"
	"runtime"
)

func main() {
	fmt.Println("Hello, world!")
	fmt.Println("Runtime:", runtime.GOOS)
}
'''

print("=== CONTENT ===")
print(repr(content))
print("\n=== ANALYSIS ===")

# Test the exact patterns from generateImports
hasRuntime = "runtime." in content or '"runtime"' in content
print(f"hasRuntime (current logic): {hasRuntime}")

# Let's test various patterns
patterns = [
    'runtime.',
    '"runtime"',
    '\t"runtime"',
    'runtime.GOOS'
]

for pattern in patterns:
    print(f"Pattern '{pattern}' in content: {pattern in content}")

# Let's see the exact lines
lines = content.split('\n')
print("\n=== LINES ===")
for i, line in enumerate(lines):
    print(f"Line {i}: {repr(line)}")